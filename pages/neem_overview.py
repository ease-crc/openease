import re
import markdown2

from datetime import datetime
from PIL import Image
from flask import redirect, url_for, render_template, flash
from furl import furl
from pathlib2 import Path
from threading import Lock

from config.settings import DATETIME_FORMAT, WEBROB_PATH, CONTENT_DIR_PATH, STATIC_DIR_PATH, DEFAULT_FILES_PATH, DOWNLOADS_DIR_PATH
from helpers.utility import download_file, sanitize_html
from helpers.file_handler import copy_file, copy_dir, get_file_extension, get_path_name, path_is_file, remove_if_is_dir, remove_if_is_file, unzip_file, dump_dict_to_json, get_dict_from_json, read_file, write_non_binary_file, make_archive_of_files_and_dirs
from helpers.thread_handler import start_thread, mutex_lock
from neems.neem import DEFAULT_IMAGE_PATH, DEFAULT_IMAGE_PATH_NO_STATIC

from app_and_db import app, db
from postgres.content import NeemOverviewData
from postgres.db import table_empty
from postgres.settings import DATETIME_MIN, ContentSettings, ContentState, UpdateMethod

NEEM_OVERVIEW_PATH = CONTENT_DIR_PATH + 'neem-overview/'
NEEM_OVERVIEW_DATA_PATH = NEEM_OVERVIEW_PATH + 'neem_overview_data.json'
NEEM_OVERVIEW_MARKDOWNS_PATH = NEEM_OVERVIEW_PATH + 'neem-overview-markdowns/'
# NEEM_OVERVIEW_IMAGES_PATH needs the subdir 'images', because otherwise
# there might be issues due to the parent folder being volume path. The
# program does file operations that would error out, if there is no subdir.
NEEM_OVERVIEW_IMAGES_PATH =  'img/neem-overview/images/'
NEEM_OVERVIEW_IMAGES_STATIC_DIR_PATH = STATIC_DIR_PATH + NEEM_OVERVIEW_IMAGES_PATH

DEFAULT_NEEM_OVERVIEW_DATA_PATH = DEFAULT_FILES_PATH + 'default_neem_overview_data.json'
DEFAULT_NEEM_OVERVIEW_IMAGES_PATH = DEFAULT_FILES_PATH + 'neem-overview-images'
DEFAULT_NEEM_OVERVIEW_MARKDOWNS_PATH = DEFAULT_FILES_PATH + 'neem-overview-markdowns'
DEFAULT_OVERVIEW_ZIP_PATH = DEFAULT_FILES_PATH + 'default_neem_overview.zip'

DOWNLOADS_DIR_NEEM_OVERVIEW_DATA = DOWNLOADS_DIR_PATH + 'neem_overview_data.json'
DOWNLOADS_DIR_NEEM_OVERVIEW_MDS_AND_IMGS = DOWNLOADS_DIR_PATH + 'neem_overview_markdowns.zip'
DOWNLOADS_DIR_NEEM_OVERVIEW_ZIP = DOWNLOADS_DIR_PATH + 'neem_overview.zip'

CURR_IMG_DIR = ''
CURR_NEEM_REPO = ''

FEATURED_NEEM_IDS = [
    '601042627e765711e2c10ab0',
    '603127322113d53026863697'
]

OVERVIEW_MUTEX = Lock()

@app.route('/overview/<neem_path>')
def render_neem_overview_page(neem_path=None):
    """ When tags or items from the markdown are not displayed correctly,
    it might hint to the sanitizer removing unallowed tags. To allow 
    these tags to pass, adjust the sanitizer-config from get_sanitizer()
    in # pages/overview.py. Afterwards adjust the styling in 
    static/css/overview.scss.
    
    When in doubt, refer to
      https://github.com/trentm/python-markdown2
    and
      https://github.com/matthiask/html-sanitizer """
    
    neem_data = get_neem_data_from_repo_path(neem_path)
    
    if not neem_data:
        app.logger.error('Could not retrieve neem data for selected neem.')
        _flash_cannot_display_overview_page()
        return redirect(url_for('render_homepage'))

    try:
        file_str = read_file(_get_local_neem_markdown_path(neem_data.neem_repo_path))
    except IOError as e:
        app.logger.error('Could not find markdown-file for neem, therefore cannot render the overview page.\n\n' + e.message)
        _flash_cannot_display_overview_page()
        return redirect(url_for('render_homepage'))

    md_content = _convert_md_to_html_and_sanitize(file_str)
    
    return render_template('pages/overview.html', **locals())


def _flash_cannot_display_overview_page():
    flash('Our apologies! Could not load the selected overview page. Please try again later!', "warning")


def _convert_md_to_html_and_sanitize(md_str):
    md_content = _convert_md_to_html(md_str)
    return sanitize_html(md_content)


def _convert_md_to_html(md_str):
    # markdown to html-conversion
    md_html = markdown2.markdown(md_str, extras=['target-blank-links', 'nofollow', 'tables'])
    # add noreferrer to links; admittedely not the nicest way of doing this
    md_html = md_html.replace('rel=\"nofollow noopener\"', 'rel=\"nofollow noopener noreferrer\"')
    return md_html


def manual_update_neem_overview_files():
    '''Should only be used where the user triggers manual updates.'''
    _update_neem_overview_files()
    ContentSettings.set_last_update_type_neem_overview(UpdateMethod.MANUAL)


def automatic_update_neem_overview_files():
    '''Should only be used where updates are triggered automatically'''
    _update_neem_overview_files()
    ContentSettings.set_last_update_type_neem_overview(UpdateMethod.AUTOMATIC)


@mutex_lock(OVERVIEW_MUTEX)
def _update_neem_overview_files():
    '''
    ideally only start this function in a seperate thread with threading.start_thread

    exception is the app start-up
    '''
    # needs to be imported here, otherwise there are errors on
    # start-up for fresh installs
    from neems.neemhub import instance as neemhub

    app.logger.info('Downloading files for neems...')

    try:
        matching_neems = neemhub.get_neem_ids('', True)
        neems = list(map(lambda (x): neemhub.get_neem(x), matching_neems))
    except Exception as e:
        app.logger.error('Could not connect to Neemhub to fetch data for neems.\n\n' + e.__str__())
    else:
        try:
            _download_all_overview_markdowns(neems)
            _download_all_overview_cover_images(neems)
            _update_overview_data(neems)
        except Exception as e:
            app.logger.info('Had issues downloading files for overview-content. Loading defaults instead.\n\n' + e.__str__())
            start_thread(load_default_overview_files)
            return
    
    if table_empty(NeemOverviewData):
        app.logger.info('No overview data gathered. Loading default files instead.')
        start_thread(load_default_overview_files)
        return

    _prepare_overview_downloads()

    app.logger.info('Finished all downloads for overview pages.')

    ContentSettings.set_last_update_neem_overview(datetime.now())
    ContentSettings.set_content_type_neem_overview(ContentState.LATEST)


def _download_all_overview_markdowns(neems):
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


def _get_local_neem_markdown_path(neem_repo_path):
    return NEEM_OVERVIEW_MARKDOWNS_PATH + neem_repo_path + '.md'


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
    file_path = Path(CURR_IMG_DIR) / get_path_name(url)

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


def _download_all_overview_cover_images(neems):
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
    supported_image_types_for_compression = [
        ".jpg",
        ".jpeg",
        ".png",
        ".gif",
        ".tif",
        ".tiff",
        ".bmp"
    ]

    if get_file_extension(file_path) not in supported_image_types_for_compression:
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
    return get_file_extension(img_url)


def _get_static_folder_neem_image_folder_path(neem_name):
    return NEEM_OVERVIEW_IMAGES_PATH + neem_name


def _update_overview_data(neems):
    db.session.query(NeemOverviewData).delete()

    overview_neems = _get_overview_data_from_neem_data(neems)

    for neem in overview_neems:
        overview_entry = _create_overview_db_entry(neem)
        db.session.add(overview_entry)

    db.session.commit()


def _get_overview_data_from_neem_data(neems):
    overview_neems = [neem.get_info_with_last_updated()
                    for neem in neems
                    if _neem_has_markdown(neem)]
    
    overview_neems = [neem for neem in overview_neems
                        if neem['last_updated'] != 0]
    
    for n_data in overview_neems:
        n_data['featured'] = _neem_is_featured(n_data)
        n_data['image'] = _determine_neem_image_path(n_data)        
        n_data['recent'] = False

    overview_neems = _set_recent_neems(overview_neems)

    return overview_neems


def _neem_has_markdown(n_data):
    return path_is_file(_get_local_neem_markdown_path(n_data.neem_repo_path))


def _neem_is_featured(n_data):
    return n_data['neem_id'] in FEATURED_NEEM_IDS


def _determine_neem_image_path(n_data):
    if n_data['image'] == DEFAULT_IMAGE_PATH:
        return DEFAULT_IMAGE_PATH_NO_STATIC
    else:
        return _get_static_folder_neem_cover_image_path(n_data['image'], n_data['neem_repo_path'])


def _set_recent_neems(overview_neems):
    overview_neems.sort(reverse=True,
                        key=lambda x: x['last_updated'])
    for x in range(6):
        overview_neems[x]['recent'] = True
    return overview_neems


def _create_overview_db_entry(n_data):
    overview_entry = NeemOverviewData()

    overview_entry.neem_id = n_data['neem_id']
    overview_entry.name = n_data['name']
    overview_entry.description = n_data['description']
    overview_entry.maintainer = n_data['maintainer']
    overview_entry.downloadUrl = n_data['downloadUrl']
    overview_entry.last_updated = n_data['last_updated']
    overview_entry.recent = n_data['recent']
    overview_entry.neem_repo_path = n_data['neem_repo_path']
    overview_entry.image = n_data['image']
    overview_entry.featured = n_data['featured']

    return overview_entry


def get_homepage_overview_data():
    neem_data = {}

    neem_data['featured_neems'] = _get_featured_neems()
    neem_data['recent_neems'] = _get_recent_neems()

    return neem_data


def _get_featured_neems():
    return NeemOverviewData.query.filter_by(featured=True).all()


def _get_recent_neems():
    recent_neems = NeemOverviewData.query.filter_by(recent=True).all()
    recent_neems.sort(reverse=True, key=lambda x: x.last_updated)
    return recent_neems


def get_neem_data_from_id(s_neem_id):
    return NeemOverviewData.query.filter_by(neem_id=s_neem_id).first()


def get_neem_data_from_repo_path(s_neem_repo_path):
    return NeemOverviewData.query.filter_by(neem_repo_path=s_neem_repo_path).first()


def dump_overview_data_as_json():
    """ This method is used to dump the overview table contents
    to a file. This is useful, if overview_data.json inside
    /default_files/ should be updated, which contains the default
    data for the developer mode. You can copy files from within the
    docker container with the docker cp command or use the admin 
    content panel to download a copy of said files. For more 
    information look at load_overview_files_default(). """
    remove_if_is_file(NEEM_OVERVIEW_DATA_PATH)
    db_dict = _turn_overview_db_entries_to_dict()
    dump_dict_to_json(db_dict, NEEM_OVERVIEW_DATA_PATH)


def _turn_overview_db_entries_to_dict():
    overview_neems = []
    db_data = NeemOverviewData.query.all()

    for entry in db_data:
        ov_neem = {}

        ov_neem['neem_id'] = entry.neem_id
        ov_neem['name'] = entry.name
        ov_neem['description'] = entry.description
        ov_neem['maintainer'] = entry.maintainer
        ov_neem['downloadUrl'] = entry.downloadUrl
        ov_neem['last_updated'] = datetime.strftime(
                                        entry.last_updated,
                                        DATETIME_FORMAT)
        ov_neem['recent'] = entry.recent
        ov_neem['neem_repo_path'] = entry.neem_repo_path
        ov_neem['image'] = entry.image
        ov_neem['featured'] = entry.featured

        overview_neems.append(ov_neem)

    return { 'overview_neems':overview_neems }


@mutex_lock(OVERVIEW_MUTEX)
def load_default_overview_files():
    """ This method loads the default neem-overview files and moves
    them to the correct locations. That includes:
      - default_neem_overview_data.json, 
            which is a json copy of the default NEEM_DATA
            will be loaded by the database
      - overview-contents from default_neem_overview.zip, 
            which contains all the overview md-files
            should be placed in /opt/webapp/webrob/
      - neem-images from default_neem_overview.zip,
            which contains the neem cover and md images
            should be placed in /opt/webapp/webrob/static/img/
    
    All the mentioned files and dirs can be found inside the container
    in the given locations. The contents of the container can then be
    copied with docker cp (please check the official documentation). """

    _unzip_default_files()
    _load_default_overview_mds()    
    _load_default_overview_images()
    _load_default_overview_data()
    _prepare_overview_downloads()

    ContentSettings.set_last_update_neem_overview(DATETIME_MIN)
    ContentSettings.set_last_update_type_neem_overview(UpdateMethod.NO_UPDATE)
    ContentSettings.set_content_type_neem_overview(ContentState.DEFAULT)


def _unzip_default_files():
    _remove_previous_default_files()
    unzip_file(DEFAULT_OVERVIEW_ZIP_PATH, DEFAULT_FILES_PATH)
    app.logger.info("Unzipped default overview files.")


def _remove_previous_default_files():
    remove_if_is_dir(DEFAULT_NEEM_OVERVIEW_MARKDOWNS_PATH)
    remove_if_is_dir(DEFAULT_NEEM_OVERVIEW_IMAGES_PATH)


def _load_default_overview_mds():
    # necessary if the container is restarted after having been put on pause
    remove_if_is_dir(NEEM_OVERVIEW_MARKDOWNS_PATH)
    copy_dir(DEFAULT_NEEM_OVERVIEW_MARKDOWNS_PATH, NEEM_OVERVIEW_MARKDOWNS_PATH)
    app.logger.info("Loaded default overview markdowns.")


def _load_default_overview_images():
    # necessary if the container is restarted after having been put on pause
    remove_if_is_dir(NEEM_OVERVIEW_IMAGES_STATIC_DIR_PATH)
    copy_dir(DEFAULT_NEEM_OVERVIEW_IMAGES_PATH, NEEM_OVERVIEW_IMAGES_STATIC_DIR_PATH)
    app.logger.info("Loaded default overview images.")


def _load_default_overview_data():
    db.session.query(NeemOverviewData).delete()
    overview_neems = get_dict_from_json(DEFAULT_NEEM_OVERVIEW_DATA_PATH)['overview_neems']
    overview_neems = _turn_datetime_strs_to_objs(overview_neems)

    for neem in overview_neems:
        overview_entry = _create_overview_db_entry(neem)
        db.session.add(overview_entry)

    db.session.commit()

    app.logger.info("Loaded default overview database.")


def _turn_datetime_strs_to_objs(overview_neems):
    for neem in overview_neems:
        neem['last_updated'] = datetime.strptime(
            neem['last_updated'],
            DATETIME_FORMAT
        )

    return overview_neems


def _prepare_overview_downloads():
    if not ContentSettings.get_settings().prepare_downloadable_files:
        app.logger.info('Config set to not prepare downloadable files.\nWill not prepare downloadable overview-files.')
        return

    app.logger.info('Preparing downloadable files for overview-pages.')

    dump_overview_data_as_json()
    _prepare_overview_data_download()
    _prepare_overview_mds_and_imgs_download()
    _prepare_overview_zip_download()

    app.logger.info('Finished preparing downloadable files for overview-pages.')


def _prepare_overview_data_download():
    remove_if_is_file(DOWNLOADS_DIR_NEEM_OVERVIEW_DATA)
    copy_file(NEEM_OVERVIEW_DATA_PATH, DOWNLOADS_DIR_NEEM_OVERVIEW_DATA)


def _prepare_overview_mds_and_imgs_download():
    remove_if_is_file(DOWNLOADS_DIR_NEEM_OVERVIEW_MDS_AND_IMGS)
    make_archive_of_files_and_dirs([
            NEEM_OVERVIEW_MARKDOWNS_PATH,
            NEEM_OVERVIEW_IMAGES_STATIC_DIR_PATH
        ], DOWNLOADS_DIR_NEEM_OVERVIEW_MDS_AND_IMGS)


def _prepare_overview_zip_download():
    remove_if_is_file(DOWNLOADS_DIR_NEEM_OVERVIEW_ZIP)
    make_archive_of_files_and_dirs([
            NEEM_OVERVIEW_DATA_PATH,
            NEEM_OVERVIEW_MARKDOWNS_PATH,
            NEEM_OVERVIEW_IMAGES_STATIC_DIR_PATH
        ], DOWNLOADS_DIR_NEEM_OVERVIEW_ZIP)
