import re

from datetime import datetime
from threading import Lock
from flask import redirect, url_for, render_template, send_from_directory, flash
from postgres.content import PublicationsData, PublicationsKeywords, keywords_association_table
from postgres.db import table_empty
from postgres.settings import DATETIME_MIN, ContentSettings, ContentState, UpdateMethod
from pybtex import PybtexEngine             # https://docs.pybtex.org/api/formatting.html#python-api
from pybtex.database import parse_file      # https://docs.pybtex.org/api/parsing.html#reading-bibliography-data
from pylatexenc.latex2text import LatexNodes2Text   # https://pypi.org/project/pylatexenc/

from app_and_db import app, db
from config.settings import CONTENT_DIR_PATH, DEFAULT_FILES_PATH, DOWNLOADS_DIR_PATH, LOCAL_PUBLICATIONS_AND_PAPERS
from helpers.utility import download_file, is_url
from helpers.file_handler import copy_file, dir_has_any_items, get_file_extension, get_path_parent, make_dir, move_file, path_is_dir, path_is_file, remove_file, remove_if_is_dir, remove_if_is_file, unzip_file, dump_dict_to_json, get_dict_from_json, make_archive_of_files_and_dirs
from helpers.thread_handler import start_thread, mutex_lock

PUBLICATIONS_DIR_PATH = CONTENT_DIR_PATH + 'publications/'
PUBLICATIONS_DATA_PATH = PUBLICATIONS_DIR_PATH + 'publications_data.json'
ALL_PUBLICATIONS_PATH = PUBLICATIONS_DIR_PATH + 'all_publications.bib'
TEST_PUBLICATIONS_PATH = PUBLICATIONS_DIR_PATH + 'test_publications.bib'
PAPERS_ZIP_PATH = PUBLICATIONS_DIR_PATH + 'papers.zip'
PAPERS_PATH = PUBLICATIONS_DIR_PATH + 'papers/'

DEFAULT_PUBLICATIONS_DATA_PATH = DEFAULT_FILES_PATH + 'default_publications_data.json'
DEFAULT_PUBLICATIONS_PATH = DEFAULT_FILES_PATH + 'default_publications.bib'
DEFAULT_PAPERS_ZIP_PATH = DEFAULT_FILES_PATH + 'default_papers.zip'

DOWNLOADS_DIR_PUBLICATIONS_DATA = DOWNLOADS_DIR_PATH + 'publications_data.json'
DOWNLOADS_DIR_PUBLICATIONS_BIBTEX = DOWNLOADS_DIR_PATH + 'publications.bib'
DOWNLOADS_DIR_PAPERS_ZIP = DOWNLOADS_DIR_PATH + 'papers.zip'
DOWNLOADS_DIR_PUBLICATIONS_AND_PAPERS_ZIP = DOWNLOADS_DIR_PATH + 'publications_and_papers.zip'

PUBLICATIONS_KEYWORDS = [
    ('openease_overview', 'Overview: Cognition-enabled Control'),
    ('openease_kb_of_exp_data', 'Knowledge Bases of Robot Experience Data'),
    ('openease_cram', 'Cognitive Robot Abstract Machine'),
    ('openease_knowledge_representation', 'Knowledge Representation and Processing'),
    ('openease_perception', 'Perception'),
    ('openease_human_activity', 'Human Activity Models'),
    ('openease_manipulation', 'Manipulation and Control'),
    ('openease_natural_language', 'Natural-language Instruction Interpretation')
]

PUBLICATIONS_MUTEX = Lock()

@app.route('/publications')
def render_all_publications():
    p_keywords = get_publications_keywords()
    show_pdf_field = _papers_dir_not_empty()

    for keyword in p_keywords:
        keyword.publications.sort(key=lambda x: x.year, reverse=True)
    
    return render_template('pages/publications.html', **locals())


def _papers_dir_not_empty():
    if path_is_dir(PAPERS_PATH):
        return dir_has_any_items(PAPERS_PATH)
    else:
        return False


def _papers_dir_empty():
    return not _papers_dir_not_empty()


@app.route('/publications/<publication_key>')
def render_bibtex_entry(publication_key=None):
    try:
        publication = get_publication_by_key(publication_key)
    except Exception as e:
        flash('Could not find the specified publication.')
        return redirect(url_for('render_all_publications'))
    
    show_pdf_field = _papers_dir_not_empty()

    return render_template('pages/bibtex.html', **locals())


@app.route('/papers/<paper>')
def get_paper(paper=None):
    if _papers_dir_empty():
        flash('Currently no papers available to load.')
        return redirect(url_for('render_all_publications'))
    
    if not path_is_file(PAPERS_PATH + paper):
        flash('Cannot find requested paper.')
        return redirect(url_for('render_all_publications'))

    return send_from_directory(PAPERS_PATH, paper)


def manual_update_publications_and_papers():
    '''Should only be used where the user triggers manual updates.'''
    _update_publications_and_papers()
    ContentSettings.set_last_update_type_publications_and_papers(UpdateMethod.MANUAL)


def automatic_update_publications_and_papers():
    '''Should only be used where updates are triggered automatically'''
    _update_publications_and_papers()
    ContentSettings.set_last_update_type_publications_and_papers(UpdateMethod.AUTOMATIC)


# ideally only start this function in a seperate thread with utility.start_thread
# exception is the app start-up
@mutex_lock(PUBLICATIONS_MUTEX)
def _update_publications_and_papers():
    content_settings = ContentSettings.get_settings()
    if content_settings.publications_bibtex_url == '':
        app.logger.info('Cannot update publications-data if no URL for the publications bibtex is provided.\nUpdate the content settings and retry.\nPublications update job will also be paused if active.')

        from helpers.background_scheduler import pause_publications_job
        pause_publications_job()
        return

    # papers need to be loaded before (!) the publications
    try:
        _fetch_and_unzip_papers()
        _fetch_and_update_bibtex()
        _update_publications_data()
    except Exception as e:
        app.logger.error('Had issues fetching papers and files for publications.' + e.__str__())
        start_thread(load_default_publications_and_papers)
        return
    
    if table_empty(PublicationsData) or table_empty(PublicationsKeywords):
        app.logger.info('No publications data gathered. Loading default files instead.')
        start_thread(load_default_publications_and_papers)
        return

    _prepare_publications_downloads()

    app.logger.info('Finished all downloads for publications pages.')

    ContentSettings.set_last_update_publications_and_papers(datetime.now())
    ContentSettings.set_content_type_publications(ContentState.LATEST)


def _fetch_and_unzip_papers():
    papers_zip_url = _get_papers_zip_url()
    if is_url(papers_zip_url):
        # fetch papers.zip from url
        _download_papers()
    else:
        if not path_is_file(LOCAL_PUBLICATIONS_AND_PAPERS + papers_zip_url):
            app.logger.info('Couldn\'t find local papers.zip. Trying default-papers.')
            flash('Couldn\'t find local papers.zip. Trying default-papers.')
            _load_default_papers()
            return
        # need to initally create the dir, otherwise there is an error
        make_dir(get_path_parent(PAPERS_ZIP_PATH), make_parents=True, path_exist_ok=True)
        # copy local zip, usually generated by external services
        copy_file(LOCAL_PUBLICATIONS_AND_PAPERS + papers_zip_url, PAPERS_ZIP_PATH)
    
    if not path_is_file(PAPERS_ZIP_PATH):
        return
    
    _clear_papers_dir()
    _unzip_papers()

    ContentSettings.set_content_type_papers(ContentState.LATEST)


def _download_papers():
    papers_zip_url = _get_papers_zip_url()
    if not is_url(papers_zip_url) or \
        not get_file_extension(papers_zip_url) == '.zip' or \
        not papers_zip_url:
        return
    
    download_file(_get_papers_zip_url(), PAPERS_ZIP_PATH)


def _get_papers_zip_url():
    return ContentSettings.get_settings().papers_zip_url


def _clear_papers_dir():
    remove_if_is_dir(PAPERS_PATH)


def _unzip_papers():
    unzip_file(PAPERS_ZIP_PATH, PAPERS_PATH)


def _fetch_and_update_bibtex():
    # the following also downloads the bibtex file
    if _new_publications_has_no_errors():
        move_file(TEST_PUBLICATIONS_PATH, ALL_PUBLICATIONS_PATH, overwrite=True)


def _new_publications_has_no_errors():
    try:
        bibtex_url = _get_publications_bibtex_url()
        if is_url(bibtex_url):
            download_file(bibtex_url, TEST_PUBLICATIONS_PATH)
        else:
            if not path_is_file(LOCAL_PUBLICATIONS_AND_PAPERS + bibtex_url):
                flash('Couldn\'t find local publications.bib.\nLoading defaults instead.')
                raise IOError('Couldn\'t find local publications.bib.\nLoading defaults instead.')
            # copy local bib, usually generated by external services
            copy_file(LOCAL_PUBLICATIONS_AND_PAPERS + bibtex_url, TEST_PUBLICATIONS_PATH)
        bibtex_db = parse_file(TEST_PUBLICATIONS_PATH)
    except Exception as e:
        app.logger.info('Had issues loading the new bibtex-file with pybtex. Check for errors and update the svn-repo. Using the old bibtex-file for now.\n\n' + e.__str__())
        remove_file(TEST_PUBLICATIONS_PATH)
        return False

    return True


def _get_publications_bibtex_url():
    return ContentSettings.get_settings().publications_bibtex_url


def _load_bibtex_db_from_file(file_path):
    try:
        bibtex_db = parse_file(file_path)
    except Exception as e:
        app.logger.error('Parsing the bibtex-database failed.\n\n' + e.__str__())
        bibtex_db = {}
    finally:
        return bibtex_db


def _update_publications_data(bibtex_db):
    _reset_publications_db_tables()
    _update_keywords_table()
    _update_publications_data()


def _reset_publications_db_tables():
    db.session.query(PublicationsKeywords).delete()
    db.session.query(PublicationsData).delete()
    db.session.commit()


def _update_keywords_table():
    for entry in PUBLICATIONS_KEYWORDS:
        keyword = PublicationsKeywords()
        keyword.name = entry[0]
        keyword.title = entry[1]
        db.session.add(keyword)
    
    db.session.commit()


def _update_publications_data():
    bibtex_db = _load_bibtex_db_from_file(ALL_PUBLICATIONS_PATH)

    if bibtex_db is None or bibtex_db == []:
        return

    # load here only once to avoid multiple db accesses
    p_keywords = get_publications_keywords()    

    for key in bibtex_db.entries:
        if 'keywords' in bibtex_db.entries[key].fields:
            curr_db_entry = bibtex_db.entries[key]
            pub_entry = PublicationsData()

            pub_entry.keywords = _get_db_entry_keywords(curr_db_entry, p_keywords)

            if not pub_entry.keywords:
                continue

            pub_entry.key = _get_db_entry_key(curr_db_entry)
            pub_entry.year = _get_db_entry_year(curr_db_entry)
            pub_entry.authors = _get_db_entry_authors(curr_db_entry)
            pub_entry.abstract = _get_db_entry_abstract(curr_db_entry)
            pub_entry.has_pdf = _check_if_publication_key_has_pdf(pub_entry.key)

            pub_entry.title = _get_db_entry_title(curr_db_entry)
            # Need to adjust title of the entry to have
            # proper capitalization of the text
            _correctly_set_db_entry_title(curr_db_entry, pub_entry.title)

            pub_entry.doi, pub_entry.url = _get_db_entry_doi_and_url(curr_db_entry)
            # Remove url and doi fields from pybtech db-entry
            # because otherwise the html-string provided by
            # pybtex will also include the link, but we provide
            # our own links -> no need for duplication 
            _remove_db_entry_url_and_doi_fields(curr_db_entry)

            # these four methods need to be called last
            # as they depends on changes made to the
            # pybtex db-entries by the previous methods,
            # namely:
            #   - _correctly_set_db_entry_title()
            #   - _remove_db_entry_url_and_doi_fields()
            pub_entry.bibtex_str = _get_db_entry_bibtex_str(curr_db_entry)
            pub_entry.bibtex_html_str = _get_db_entry_bibtex_html_str(curr_db_entry)
            pub_entry.html_str = _get_db_entry_html_str(curr_db_entry)
            pub_entry.reference_str = _get_db_entry_reference_str(curr_db_entry)

            db.session.add(pub_entry)
    
    db.session.commit()


def _get_db_entry_key(db_entry):
    return db_entry.key


def _get_db_entry_title(db_entry):
    title = db_entry.fields['title']
    
    # due to latex & bibtex stuff the title can include
    # certain unwanted characters, so they need to be removed
    title = re.sub(r'[{}]', '', title)
    title = title.strip('"')
    
    return title


def _correctly_set_db_entry_title(db_entry, title):
    db_entry.fields['title'] = '{' + title + '}'
                        

def _get_db_entry_authors(db_entry):
    authors = ''
    name_parts = ['first',
                'middle',
                'prelast',
                'last',
                'lineage']
    
    for author in db_entry.persons['author']:
        auth_name = ''
        for part in name_parts:
            if author.get_part_as_text(part) != '':
                auth_name += author.get_part_as_text(part) + ' '
        
        authors += _latex_to_text(auth_name.strip()) + ', '

    # remove comma and white space from last entry
    return authors[:-2]


def _get_db_entry_abstract(db_entry):
    abstract = ''
    
    if 'abstract' in db_entry.fields:
        abstract = _latex_to_text(db_entry.fields['abstract'])

    return abstract


def _check_if_publication_key_has_pdf(entry_key):
    paper_path = PAPERS_PATH + entry_key.encode('utf-8') + '.pdf'

    if path_is_file(paper_path):
        return True
    
    return False


def _get_db_entry_doi_and_url(db_entry):
    doi = ''
    url = ''

    if 'doi' in db_entry.fields:
        doi = db_entry.fields['doi']
    
    if 'url' in db_entry.fields:
        url = db_entry.fields['url']
        
        if doi != '' and doi in url:
            url = ''

    return doi, url


def _remove_db_entry_url_and_doi_fields(db_entry):
    if 'doi' in db_entry.fields:
        db_entry.fields.pop('doi')
    if 'url' in db_entry.fields:
        db_entry.fields.pop('url')


def _get_db_entry_keywords(db_entry, p_keywords):
    keyword_list = []

    for keyword in p_keywords:
        if keyword.name in db_entry.fields['keywords']:
            keyword_list.append(keyword)

    return keyword_list


def _get_db_entry_bibtex_str(db_entry):
    return db_entry.to_string('bibtex')


def _get_db_entry_bibtex_html_str(db_entry):
    bibtex_str = _get_db_entry_bibtex_str(db_entry)

    # replace bibtex line_breaks and tabs for html-equivalents
    bibtex_str = re.sub(r'\n', '<br>', bibtex_str)
    bibtex_str = re.sub(r'    ', '&nbsp;&nbsp;&nbsp;&nbsp;', bibtex_str)
    
    return bibtex_str


def _get_db_entry_year(db_entry):
    year = 0

    if 'year' in db_entry.fields:
        year = int(db_entry.fields['year'])
    
    return year


def _get_db_entry_reference_str(db_entry):
    reference_str = _get_db_entry_formatted_str(db_entry, 'text')
    reference_str = re.search(r'\[.*?\] (.*)', reference_str).group(1)

    return reference_str


def _get_db_entry_html_str(db_entry):
    html_str = _get_db_entry_formatted_str(db_entry, 'html')
    html_str = re.search(r'<dd>(.*?)</dd>', html_str, re.DOTALL).group(1)

    # certain characters are not converted properly from
    # latex to html, so it needs to be done manually
    html_str = _remove_latex_artifacts_from_html_str(html_str)

    return html_str


def _get_db_entry_formatted_str(db_entry, output_type):
    # possible output types: html, text
    pb_engine = PybtexEngine()
    bibtex_str = _get_db_entry_bibtex_str(db_entry)

    return pb_engine.format_from_string(bibtex_str, style='unsrt', output_backend=output_type, bib_encoding='bibtex')


def _remove_latex_artifacts_from_html_str(html_str):
    # certain characters are not converted properly from
    # latex to html by the pybtex library, so it needs to
    # be done manually and in a cumbersome fashion...
    #
    # might need to be extended in the future
    clean_html_str = re.sub(r'[\\][\\]textasciitilde[&]nbsp[;][<]span class[=]["]bibtex[-]protected["][>]a[<][/]span[>]', 'a&#771;', html_str)
    clean_html_str = re.sub(r'[\\][\\][&]', '&', clean_html_str)

    return clean_html_str


def _latex_to_text(tex):
    return LatexNodes2Text().latex_to_text(tex)


@mutex_lock(PUBLICATIONS_MUTEX)
def load_default_publications_and_papers():
    """ This method loads the contents of publications_data.json and extracts
    the contents of paper.zip and moves them to the correct locations.
    
    papers.zip contains the pdf which should be linked to the entries
    on the publications page. papers.zip should be extracted to
    
      PAPERS_PATH = '/opt/webapp/webrob/content/publications/papers/'
    
    All the mentioned files and dirs can be found inside the container
    in the given locations. The contents of the container can then be
    copied with docker cp (please check the official documentation).
    
    papers need to be loaded before (!) the publications """
    
    _load_default_papers()
    _load_default_publications()

    _prepare_publications_downloads()

    ContentSettings.set_last_update_publications_and_papers(DATETIME_MIN)
    ContentSettings.set_last_update_type_publications_and_papers(UpdateMethod.NO_UPDATE)


def _load_default_papers():
    download_default_papers = True if ContentSettings.get_settings().download_default_papers else False

    if not path_is_file(DEFAULT_PAPERS_ZIP_PATH):
        # check for local copy in /opt/webapp/webrob/publications_and_papers/
        default_papers_url = _get_default_papers_zip_url()
        if default_papers_url and not is_url(default_papers_url):
            if path_is_file(LOCAL_PUBLICATIONS_AND_PAPERS + default_papers_url):
                copy_file(LOCAL_PUBLICATIONS_AND_PAPERS + default_papers_url, DEFAULT_PAPERS_ZIP_PATH)
            else:
                _log_failed_to_find_local_default_papers()
                ContentSettings.set_content_type_papers(ContentState.NONE)
        elif download_default_papers or not app.config['DEBUG']:
            if is_url(default_papers_url):
                download_file(default_papers_url, DEFAULT_PAPERS_ZIP_PATH)

            if not path_is_file(DEFAULT_PAPERS_ZIP_PATH):
                _log_failed_default_papers_download()
                ContentSettings.set_content_type_papers(ContentState.NONE)
                return
        else:
            _log_could_not_find_default_papers()
            ContentSettings.set_content_type_papers(ContentState.NONE)
            return
    else:
        _log_loading_cached_default_papers()

    try:
        _clear_papers_dir()
        unzip_file(DEFAULT_PAPERS_ZIP_PATH, PAPERS_PATH)
        app.logger.info("Loaded and extracted default papers.")
        ContentSettings.set_content_type_papers(ContentState.DEFAULT)
    except Exception as e:
        app.logger.warning('Had issues unzipping default papers.\n\n' + e.__str__())


def _get_default_papers_zip_url():
    return ContentSettings.get_settings().default_papers_zip_url


def _log_loading_cached_default_papers():
    message = 'Using cached default_papers.zip.'
    app.logger.info(message)
    flash(message)


def _log_failed_to_find_local_default_papers():
    message = 'Couldn\'t find local default_papers.zip.'
    app.logger.info(message)
    flash(message)


def _log_failed_default_papers_download():
    app.logger.info("Download of default_papers.zip failed. Check if the URL is correct or instead try to manually download the file, place it into /default_files and rebuild.")


def _log_could_not_find_default_papers():
    app.logger.info("Could not find default_papers.zip and settings are configured not to download default_papers.zip, therefore not loading any default papers.\n\nIf you wish to view papers on the publications-page during development, you have two option:\n1. Only if you have an admin account: Go to the admin's content-settings page, provide a download link for the default_papers.zip, and set DOWNLOAD_DEFAULT_PAPERS to ON, then click on 'Load Defaults'. This will download the zip.file into the running container and update all the necessary data-structures. Afterwards turn the setting off again. The file be retained until the 'content'-volume is deleted, thus you do not need to repeat this step on rebuilds. WARNING: If you forget to turn off the setting again, the container will download the papers on each rebuild. This will increase build times a lot.\n2. Manually download the zip.file and put it into /default_files and re-run docker-compose up. This will increase build times a bit.")


def _load_default_publications():
    _reset_publications_db_tables()
    _load_default_publications_data_from_json()
    app.logger.info("Loaded default publications database.")

    ContentSettings.set_content_type_publications(ContentState.DEFAULT)


def _load_default_publications_data_from_json():
    json_data = get_dict_from_json(DEFAULT_PUBLICATIONS_DATA_PATH)
    p_data = json_data['publications_data']
    p_keywords = json_data['publications_keywords']

    for entry in p_keywords:
        kw_entry = PublicationsKeywords()

        kw_entry.name = entry['name']
        kw_entry.title = entry['title']

        db.session.add(kw_entry)

    db.session.commit() 

    # load here only once to avoid multiple db accesses
    p_keywords = get_publications_keywords()  

    p_dir_not_empty = _papers_dir_not_empty()

    for entry in p_data:
        pub_entry = PublicationsData()

        pub_entry.key = entry['key']
        pub_entry.year = entry['year']
        pub_entry.authors = entry['authors']
        pub_entry.abstract = entry['abstract']
        if p_dir_not_empty:
            pub_entry.has_pdf = _check_if_publication_key_has_pdf(entry['key'])
        pub_entry.title = entry['title']
        pub_entry.doi = entry['doi']
        pub_entry.url = entry['url']
        pub_entry.bibtex_str = entry['bibtex_str']
        pub_entry.bibtex_html_str = entry['bibtex_html_str']
        pub_entry.html_str = entry['html_str']
        pub_entry.reference_str = entry['reference_str']
        
        for kw in p_keywords:
            if kw.name in entry['keywords']:
                pub_entry.keywords.append(kw)

        db.session.add(pub_entry)

    db.session.commit() 


def dump_publications_data_as_json():
    """ This method is used to dump the PUBLICATIONS_DATA dict to a
    file. This is useful when the publications_data.json 
    should be updated, which contains the default data for the
    developer mode. You can copy files from within the docker 
    container with the docker cp command. For more information
    look at load_default_publications_and_papers(). """

    remove_if_is_file(PUBLICATIONS_DATA_PATH)
    db_dict = _turn_publications_db_entries_to_dict()
    dump_dict_to_json(db_dict, PUBLICATIONS_DATA_PATH)


def _turn_publications_db_entries_to_dict():
    return {
        'publications_keywords':_turn_publications_keyword_entries_to_dict(),
        'publications_data':_turn_publications_data_entries_to_dict()
    }


def _turn_publications_keyword_entries_to_dict():
    publication_keywords = []
    db_data = get_publications_keywords()

    for entry in db_data:
        p_keyword = {}

        p_keyword['name'] = entry.name
        p_keyword['title'] = entry.title

        publication_keywords.append(p_keyword)

    return publication_keywords


def _turn_publications_data_entries_to_dict():
    publications = []
    db_data = PublicationsData.query.all()

    for entry in db_data:
        pub = {}

        pub['key'] = entry.key
        pub['title'] = entry.title
        pub['authors'] = entry.authors
        pub['year'] = entry.year
        pub['abstract'] = entry.abstract
        pub['has_pdf'] = entry.has_pdf
        pub['doi'] = entry.doi
        pub['url'] = entry.url
        pub['bibtex_str'] = entry.bibtex_str
        pub['bibtex_html_str'] = entry.bibtex_html_str
        pub['html_str'] = entry.html_str
        pub['reference_str'] = entry.reference_str
        pub['keywords'] = []
        
        # add references to keywords from the association table
        for keyword in entry.keywords:
            pub['keywords'].append(keyword.name)
        
        publications.append(pub)
    
    return publications


def _prepare_publications_downloads():
    if not ContentSettings.get_settings().prepare_downloadable_files:
        app.logger.info('Config set to not prepare downloadable files.\nWill not prepare downloadable publication-files.')
        return

    app.logger.info('Preparing downloadable files for publications & papers.')

    if not path_is_dir(PUBLICATIONS_DIR_PATH):
        make_dir(PUBLICATIONS_DIR_PATH, make_parents=True)

    dump_publications_data_as_json()
    _prepare_publications_data_download()
    _prepare_publications_bibtex_download()
    _prepare_papers_download()
    _prepare_publications_zip_download()

    app.logger.info('Finished preparing downloadable files for publications & papers.')


def _prepare_publications_data_download():
    remove_if_is_file(DOWNLOADS_DIR_PUBLICATIONS_DATA)
    copy_file(PUBLICATIONS_DATA_PATH, DOWNLOADS_DIR_PUBLICATIONS_DATA)


def _prepare_publications_bibtex_download():
    bibtex_path = _get_current_bibtex_path()
    remove_if_is_file(DOWNLOADS_DIR_PUBLICATIONS_BIBTEX)
    copy_file(bibtex_path, DOWNLOADS_DIR_PUBLICATIONS_BIBTEX)


def _get_current_bibtex_path():
    return ALL_PUBLICATIONS_PATH if path_is_file(ALL_PUBLICATIONS_PATH) else DEFAULT_PUBLICATIONS_PATH


def _prepare_papers_download():
    if _papers_dir_empty():
        app.logger.info('No papers found, so papers.zip won\'t be created for downloads.')
        return
    
    remove_if_is_file(DOWNLOADS_DIR_PAPERS_ZIP)
    make_archive_of_files_and_dirs([
        PAPERS_PATH
    ], DOWNLOADS_DIR_PAPERS_ZIP)


def _prepare_publications_zip_download():
    bibtex_path = _get_current_bibtex_path()

    path_list = [
        PUBLICATIONS_DATA_PATH,
        bibtex_path
    ]

    if _papers_dir_not_empty():
        path_list.append(PAPERS_PATH)
    else:
        app.logger.info('No papers found, so no papers added to publications.zip.')
    
    remove_if_is_file(DOWNLOADS_DIR_PUBLICATIONS_AND_PAPERS_ZIP)
    make_archive_of_files_and_dirs(path_list, DOWNLOADS_DIR_PUBLICATIONS_AND_PAPERS_ZIP)


def get_publication_by_key(publication_key):
    return PublicationsData.query.filter_by(key=publication_key).first()


def get_publications_keywords():
    return PublicationsKeywords.query.all()
