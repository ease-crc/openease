from flask import render_template, send_file
from flask.helpers import flash
from pathlib2 import Path

from app_and_db import app
from helpers.utility import admin_required, start_thread
from pages.neem_overview import DOWNLOADS_DIR_OVERVIEW_DATA, DOWNLOADS_DIR_OVERVIEW_MDS_AND_IMGS, DOWNLOADS_DIR_OVERVIEW_ZIP, download_neem_files, load_default_overview_files
from pages.publications import DOWNLOADS_DIR_PAPERS_ZIP, DOWNLOADS_DIR_PUBLICATIONS_BIBTEX, DOWNLOADS_DIR_PUBLICATIONS_DATA, DOWNLOADS_DIR_PUBLICATIONS_ZIP, download_and_update_papers_and_bibtex, load_default_publications_and_papers

@app.route('/settings/content')
@admin_required
def render_content_settings():
    return render_template('settings/content.html', **locals())

# TODO
# - Error Testing all of this stuff
# - settings and updates

def manually_load_resource(func):
    try:
        func()
    except Exception as e:
        app.logger.error(e.__str__())
        flash('Action failed!')
    else:
        flash('Action succeeded!')


@app.route('/settings/content/update_all')
@admin_required
def manually_load_all_content_updates():
    start_thread(manually_load_resource(download_neem_files))
    start_thread(manually_load_resource(download_and_update_papers_and_bibtex))
    return render_content_settings()


@app.route('/settings/content/load_all_default')
@admin_required
def manually_load_all_default_content():
    start_thread(manually_load_resource(load_default_overview_files))
    start_thread(manually_load_resource(load_default_publications_and_papers))
    return render_content_settings()


@app.route('/settings/content/update_publications')
@admin_required
def manually_load_publications_updates():
    start_thread(manually_load_resource(download_and_update_papers_and_bibtex))
    return render_content_settings()


@app.route('/settings/content/load_default_publications')
@admin_required
def manually_load_publications_defaults():
    start_thread(manually_load_resource(load_default_publications_and_papers))
    return render_content_settings()


@app.route('/settings/content/update_overview_files')
@admin_required
def manually_load_overview_updates():
    start_thread(manually_load_resource(download_neem_files))
    return render_content_settings()


@app.route('/settings/content/load_overview_default')
@admin_required
def manually_load_overview_defaults():
    start_thread(manually_load_resource(load_default_overview_files))
    return render_content_settings()


@app.route('/settings/content/overview_data_json')
@admin_required
def send_overview_data_json():
    return _send_file_if_available(DOWNLOADS_DIR_OVERVIEW_DATA)


@app.route('/settings/content/overview_mds_and_imgs')
@admin_required
def send_overview_mds_and_imgs():
    return _send_file_if_available(DOWNLOADS_DIR_OVERVIEW_MDS_AND_IMGS)


@app.route('/settings/content/overview_zip')
@admin_required
def send_overview_zip():
    return _send_file_if_available(DOWNLOADS_DIR_OVERVIEW_ZIP)


@app.route('/settings/content/publications_data_json')
@admin_required
def send_publications_data_json():
    return _send_file_if_available(DOWNLOADS_DIR_PUBLICATIONS_DATA)


@app.route('/settings/content/publications_bib')
@admin_required
def send_publications_bibtex():
    return _send_file_if_available(DOWNLOADS_DIR_PUBLICATIONS_BIBTEX)


@app.route('/settings/content/papers_zip')
@admin_required
def send_papers_zip():
    return _send_file_if_available(DOWNLOADS_DIR_PAPERS_ZIP)


@app.route('/settings/content/publications_zip')
@admin_required
def send_publications_zip():
    return _send_file_if_available(DOWNLOADS_DIR_PUBLICATIONS_ZIP)


def _send_file_if_available(FILE_PATH):
    if not Path(FILE_PATH).is_file():
        flash('Could not retrieve requested file. Check app-settings or try again later.')
        return render_content_settings()

    return send_file(FILE_PATH, as_attachment=True)
    