import re

from PIL import Image
from furl import furl
from pathlib2 import Path
from threading import Lock
from html_sanitizer import Sanitizer
from html_sanitizer.sanitizer import sanitize_href, bold_span_to_strong,italic_span_to_em, target_blank_noopener, tag_replacer

from utility import copy_dir, copy_file, download_file, make_archive_of_files_and_dirs, mutex_lock, read_file, remove_if_is_file, unzip_file, remove_if_is_dir, write_non_binary_file, dump_dict_to_json, get_dict_from_json
from config.settings import WEBROB_PATH, STATIC_DIR_PATH, DEFAULT_FILES_PATH, DOWNLOADS_DIR_PATH
from neems.neemhub import instance as neemhub, NEEMHubConnectionError
from neems.neem import DEFAULT_IMAGE_PATH, DEFAULT_IMAGE_PATH_NO_STATIC

from app_and_db import app

NEEM_OVERVIEW_PATH = WEBROB_PATH + 'overview-contents/'
NEEM_DATA_PATH = NEEM_OVERVIEW_PATH + 'overview_data.json'
NEEM_OVERVIEW_MARKDOWNS_PATH = NEEM_OVERVIEW_PATH + 'neem-markdowns/'
NEEM_IMAGES_PATH = 'img/neem-images/'
NEEM_IMAGES_STATIC_DIR_PATH = STATIC_DIR_PATH + NEEM_IMAGES_PATH

DEFAULT_NEEM_DATA_PATH = DEFAULT_FILES_PATH + 'default_overview_data.json'
DEFAULT_NEEM_IMAGES_PATH = DEFAULT_FILES_PATH + 'neem-images'
DEFAULT_NEEM_OVERVIEW_MARKDOWNS_PATH = DEFAULT_FILES_PATH + 'neem-markdowns'
DEFAULT_OVERVIEW_ZIP_PATH = DEFAULT_FILES_PATH + 'default_overview.zip'

DOWNLOADS_DIR_OVERVIEW_DATA = DOWNLOADS_DIR_PATH + 'overview_data.json'
DOWNLOADS_DIR_OVERVIEW_MDS_AND_IMGS = DOWNLOADS_DIR_PATH + 'overview_markdowns.zip'
DOWNLOADS_DIR_OVERVIEW_ZIP = DOWNLOADS_DIR_PATH + 'overview.zip'

CURR_IMG_DIR = ''
CURR_NEEM_REPO = ''

FEATURED_NEEM_IDS = [
    '601042627e765711e2c10ab0', 
    '603127322113d53026863697'
]

SUPPORTED_IMAGE_TYPES_FOR_COMPRESSION = [
    ".jpg",
    ".jpeg",
    ".png",
    ".gif",
    ".tif",
    ".tiff",
    ".bmp"
]

OVERVIEW_MUTEX = Lock()

# for structure of NEEM_DATA check default_files/default_overview_data.json
NEEM_DATA = {}

@mutex_lock(OVERVIEW_MUTEX)
def download_neem_files():
    app.logger.info('Downloading files for neems...')

    try:
        matching_neems = neemhub.get_neem_ids('', True)
        neems = list(map(lambda (x): neemhub.get_neem(x), matching_neems))
    except Exception as e:
        app.logger.error('Could not connect to Neemhub to fetch data for neems.\n\n' + e.__str__())
    else:
        _download_all_neem_markdowns(neems)
        _download_all_neem_cover_images(neems)
        _update_neem_data(neems)

    if NEEM_DATA is None or NEEM_DATA == {}:
        app.logger.info('Neem-Data was empty or None. Loading default files instead.')
        load_default_overview_files()
        return

    _prepare_overview_downloads()

    app.logger.info('Finished all downloads for overview pages.')


def _download_all_neem_markdowns(neems):
    app.logger.info('Downloading markdown-files for neems... (and their images')

    for neem in neems:
        try:
            _download_neem_markdown(neem)
        except Exception as e:
            app.logger.warning('Had troubles downloading a markdown-file.\n\n' + e.__str__())

    app.logger.info('Finished downloading markdown-files for neems.')


def _download_neem_markdown(neem):
    url = neem.downloadUrl + '/-/raw/master/README.md'
    file_path = _get_local_neem_markdown_path(neem.neem_repo_path)
    download_file(url, file_path)
    _download_and_replace_all_md_images(neem)


def _get_local_neem_markdown_path(neem_name):
    return NEEM_OVERVIEW_MARKDOWNS_PATH + neem_name + '.md'


def _download_and_replace_all_md_images(neem):
    global CURR_IMG_DIR
    global CURR_NEEM_REPO

    # open md-file
    file_path = _get_local_neem_markdown_path(neem.neem_repo_path)
    file_str = read_file(file_path)

    CURR_IMG_DIR = '/static/'+ _get_static_folder_neem_image_folder_path(neem.neem_repo_path)
    CURR_NEEM_REPO = neem.downloadUrl

    file_str = _scan_for_html_images(file_str)
    file_str = _scan_for_md_images(file_str)
    
    CURR_IMG_DIR = ''
    CURR_NEEM_REPO = ''

    # write changes back to md-file
    write_non_binary_file(file_str, file_path)


def _scan_for_html_images(md_file_str):
    pattern = r'(?P<begin><\s*?img.*?src=")(?P<url>.*?)(?P<end>".*?>)'
    return _scan_for_images_in_md_file(pattern, md_file_str)


def _scan_for_md_images(md_file_str):
    pattern = r'(?P<begin>!\[.*?\]\()(?P<url>.*?)(?P<end>\))'
    return _scan_for_images_in_md_file(pattern, md_file_str)


def _scan_for_images_in_md_file(pattern, md_file_str):
    return re.sub(pattern, _download_image_and_replace_url, md_file_str)


def _download_image_and_replace_url(matchobj):
    url = furl(matchobj.group('url')).remove(args=True, fragment=True).url
    file_path = Path(CURR_IMG_DIR) / Path(url).name

    if not _is_weburl(url):
        neemgit_url = CURR_NEEM_REPO
        neemgit_url_suffix = '/-/raw/master/' + url

        url = neemgit_url + neemgit_url_suffix

    final_path = WEBROB_PATH + str(file_path)
    download_file(url, final_path)
    _compress_image(final_path)

    return matchobj.group('begin') + str(file_path) + matchobj.group('end')


def _is_weburl(string):
    return True if re.match('https{,1}://', string) else False


def _download_all_neem_cover_images(neems):
    app.logger.info('Downloading cover-images for neems...')

    for neem in neems:
        if neem.image == DEFAULT_IMAGE_PATH:
            continue
        else:
            _download_neem_cover_image(neem)

    app.logger.info('Finished downloading cover-images for neems.')


def _download_neem_cover_image(neem):
    url = neem.image
    file_path = _get_local_neem_cover_image_path(neem)
    download_file(url, file_path)
    _compress_image(file_path)


def _compress_image(file_path, compression_value=30):
    if Path(file_path).suffix not in SUPPORTED_IMAGE_TYPES_FOR_COMPRESSION:
        return
    
    im = Image.open(file_path)
    im.save(file_path,optimize=True,quality=compression_value)


def _get_local_neem_cover_image_path(neem):
    return STATIC_DIR_PATH + _get_static_folder_neem_cover_image_path(neem.image, neem.neem_repo_path)


def _get_static_folder_neem_cover_image_path(neem_image_url, neem_name):
    file_ending = _get_url_image_file_ending(neem_image_url)
    return _get_static_folder_neem_image_folder_path(neem_name) + '/cover/cover_img' + file_ending


def _get_url_image_file_ending(image_url):
    img_url = furl(image_url).remove(args=True, fragment=True).url
    return Path(img_url).suffix


def _get_static_folder_neem_image_folder_path(neem_name):
    return NEEM_IMAGES_PATH + neem_name


def _update_neem_data(neems):
    global NEEM_DATA
    neem_data = {}

    neem_data['all_neems'] = _get_all_neems_data_with_last_updated(neems)
    neem_data['featured_neems'] = _get_featured_neems_data(neem_data['all_neems'])
    neem_data['recent_neems'] = _get_recent_neems_data(neem_data['all_neems'])

    NEEM_DATA = neem_data


def _get_all_neems_data_with_last_updated(neems):
    all_neems = [neem.get_info_with_last_updated() for neem in neems]
    
    for n_data in all_neems:
        if n_data['image'] == DEFAULT_IMAGE_PATH:
            n_data['image'] = DEFAULT_IMAGE_PATH_NO_STATIC
        else:
            n_data['image'] = _get_static_folder_neem_cover_image_path(n_data['image'], n_data['neem_repo_path'])
        n_data['md_path'] = _get_local_neem_markdown_path(n_data['neem_repo_path'])

    return all_neems


def _get_featured_neems_data(neem_data_list):
    return [n_data 
            for n_data in neem_data_list
            if n_data['neem_id'] in FEATURED_NEEM_IDS]
    

def _get_recent_neems_data(neem_data_list):
    recent_neems = [n_data
                    for n_data in neem_data_list
                    if n_data['neem_id'] not in FEATURED_NEEM_IDS]
    # list.sort() does in-place sorting and returns None, therefore
    # it shouldn't be called when assigning or returning the list
    recent_neems.sort(reverse=True, key=lambda x: x['last_updated'])
    return recent_neems[0:6] 


def get_neem_data():
    global NEEM_DATA
    return NEEM_DATA


def get_neem_data_from_id(neem_id):
    global NEEM_DATA
    
    return next((n_data for n_data in NEEM_DATA['all_neems'] if n_data['neem_id'] == neem_id), None)


def get_neem_data_from_repo_path(neem_repo_path):
    global NEEM_DATA
    
    return next((n_data for n_data in NEEM_DATA['all_neems'] if n_data['neem_repo_path'] == neem_repo_path), None)


def dump_neem_data_as_json():
    # This method is used to dump the NEEM_DATA dict to a file.
    # This is useful, if overview_data.json inside overview.zip
    # should be updated, which contains the default data for the
    # developer mode. You can copy files from within the docker 
    # container with the docker cp command. For more information
    # look at load_overview_files_default().
    remove_if_is_file(NEEM_DATA_PATH)
    dump_dict_to_json(NEEM_DATA, NEEM_DATA_PATH)


@mutex_lock(OVERVIEW_MUTEX)
def load_default_overview_files():
    # This method loads the contents of overview.zip and moves
    # them to the correct locations.
    # overview.zip contains:
    #   - overview_data.json, which is a json copy of the default NEEM_DATA
    #       should be placed in /opt/webapp/webrob/default_files/
    #   - overview-contents, which contains all the overview md-files
    #       should be placed in /opt/webapp/webrob/
    #   - neem-images, which contains the neem cover and md images
    #       should be placed in /opt/webapp/webrob/static/img/
    # 
    # All the mentioned files and dirs can be found inside the container
    # in the given locations, if the container is run in production mode 
    # instead of developer mode. For that, make sure that docker-compose
    # is run with EASE-DEBUG set to False or the if-conditional in
    # runserver.py that calls this method is changed to 
    # 
    #   if not _config_is_debug():
    #       load_overview_files_default()
    # 
    # (do one or the other, don't do both). The contents of the container
    # can then be copied with docker cp (please check the official
    # documentation).
    _unzip_default_files()
    _load_default_overview_mds()    
    _load_default_overview_images()
    _load_default_neem_data()
    _prepare_overview_downloads()


def _unzip_default_files():
    _remove_previous_default_files()
    unzip_file(DEFAULT_OVERVIEW_ZIP_PATH, DEFAULT_FILES_PATH)
    app.logger.info("Unzipped default overview files.")


def _remove_previous_default_files():
    remove_if_is_dir(DEFAULT_NEEM_OVERVIEW_MARKDOWNS_PATH)
    remove_if_is_dir(DEFAULT_NEEM_IMAGES_PATH)


def _load_default_overview_mds():
    # necessary if the container is restarted after having been put on pause
    remove_if_is_dir(NEEM_OVERVIEW_MARKDOWNS_PATH)
    copy_dir(DEFAULT_NEEM_OVERVIEW_MARKDOWNS_PATH, NEEM_OVERVIEW_MARKDOWNS_PATH)
    app.logger.info("Loaded default overview markdowns.")


def _load_default_overview_images():
    # necessary if the container is restarted after having been put on pause
    remove_if_is_dir(NEEM_IMAGES_STATIC_DIR_PATH)
    copy_dir(DEFAULT_NEEM_IMAGES_PATH, NEEM_IMAGES_STATIC_DIR_PATH)
    app.logger.info("Loaded default overview images.")


def _load_default_neem_data():
    global NEEM_DATA

    NEEM_DATA = get_dict_from_json(DEFAULT_NEEM_DATA_PATH)
    app.logger.info("Loaded default overview database.")


def _prepare_overview_downloads():
    if app.config['DEBUG'] and not app.config['PREPARE_DOWNLOADABLE_FILES']:
        app.logger.info('Config set to not prepare downloadable files.\nWill not prepare downloadable overview-files.')
        return

    app.logger.info('Preparing downloadable files for overview-pages.')

    dump_neem_data_as_json()
    _prepare_overview_data_download()
    _prepare_overview_mds_and_imgs_download()
    _prepare_overview_zip_download()

    app.logger.info('Finished.')


def _prepare_overview_data_download():
    remove_if_is_file(DOWNLOADS_DIR_OVERVIEW_DATA)
    copy_file(NEEM_DATA_PATH, DOWNLOADS_DIR_OVERVIEW_DATA)


def _prepare_overview_mds_and_imgs_download():
    remove_if_is_file(DOWNLOADS_DIR_OVERVIEW_MDS_AND_IMGS)
    make_archive_of_files_and_dirs([
            NEEM_OVERVIEW_MARKDOWNS_PATH,
            NEEM_IMAGES_STATIC_DIR_PATH
        ], DOWNLOADS_DIR_OVERVIEW_MDS_AND_IMGS)


def _prepare_overview_zip_download():
    remove_if_is_file(DOWNLOADS_DIR_OVERVIEW_ZIP)
    make_archive_of_files_and_dirs([
            NEEM_DATA_PATH,
            NEEM_OVERVIEW_MARKDOWNS_PATH,
            NEEM_IMAGES_STATIC_DIR_PATH
        ], DOWNLOADS_DIR_OVERVIEW_ZIP)


def get_sanitizer():
    # When tags or items from the markdown are not displayed correctly,
    # it might hint to the sanitizer removing unallowed tags. To allow 
    # these tags to pass, adjust the sanitizer-config from get_sanitizer()
    # in # pages/overview.py. Afterwards adjust the styling in 
    # static/css/overview.scss.
    #
    # When in doubt, refer to
    #   https://github.com/trentm/python-markdown2
    # and
    #   https://github.com/matthiask/html-sanitizer
    
    return Sanitizer({
        'tags': {
            'a', 'h1', 'h2', 'h3', 'strong', 'em', 'p', 'ul', 'ol',
            'li', 'br', 'sub', 'sup', 'hr', 'img', 'blockquote',
            'table', 'thead', 'tbody', 'tr', 'th', 'td',
        },
        'attributes': {
            'a': ('href', 'name', 'target', 'title', 'id', 'rel'),
            'img': ('src', 'alt', 'width', 'height'),
        },
        'empty': {'hr', 'a', 'br', 'img', 'tr', 'th', 'td'},
        'separate': {
            'a', 'p', 'li', 'img', 'table', 'tr', 'th', 'td', 'blockquote',
        },
        'whitespace': {'br'},
        'keep_typographic_whitespace': False,
        'add_nofollow': False,
        'autolink': False,
        'sanitize_href': sanitize_href,
        'element_preprocessors': [
            # convert span elements into em/strong if a matching style rule
            # has been found. strong has precedence, strong & em at the same
            # time is not supported
            bold_span_to_strong,
            italic_span_to_em,
            tag_replacer('b', 'strong'),
            tag_replacer('i', 'em'),
            tag_replacer('form', 'p'),
            target_blank_noopener,
        ],
        'element_postprocessors': [],
        'is_mergeable': lambda e1, e2: True
    })
