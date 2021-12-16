from flask import render_template, send_file, redirect, url_for, request
from flask.helpers import flash
from pathlib2 import Path
from datetime import datetime

from app_and_db import app
from helpers.utility import admin_required, start_thread
from pages.neem_overview import DOWNLOADS_DIR_OVERVIEW_DATA, DOWNLOADS_DIR_OVERVIEW_MDS_AND_IMGS, DOWNLOADS_DIR_OVERVIEW_ZIP, manual_update_neem_overview_files, load_default_overview_files
from pages.publications import DOWNLOADS_DIR_PAPERS_ZIP, DOWNLOADS_DIR_PUBLICATIONS_BIBTEX, DOWNLOADS_DIR_PUBLICATIONS_DATA, DOWNLOADS_DIR_PUBLICATIONS_AND_PAPERS_ZIP, manual_update_publications_and_papers, load_default_publications_and_papers
from helpers.background_scheduler import get_neem_overview_job_next_runtime, get_publications_job_next_runtime, resume_neem_overview_job, resume_publications_job, pause_neem_overview_job, pause_publications_job
from postgres.settings import DATETIME_MIN, ContentSettings, UpdateMethod, UpdateState, ContentState

@app.route('/news')
def render_news():
    return render_template('pages/news.html', **locals())


@app.route('/settings/content')
@admin_required
def render_content_settings():
    content_settings = ContentSettings.get_settings()
    last_update_publications = _reset_datetime_microseconds_and_utc_offset(content_settings.last_update_publications_and_papers)
    next_update_publications = _reset_datetime_microseconds_and_utc_offset(get_publications_job_next_runtime())
    last_update_overview = _reset_datetime_microseconds_and_utc_offset(content_settings.last_update_neem_overview)
    next_update_overview = _reset_datetime_microseconds_and_utc_offset(get_neem_overview_job_next_runtime())
    from postgres.settings import DATETIME_MIN, UpdateMethod, UpdateState, ContentState

    papers_zip_download_exists = Path(DOWNLOADS_DIR_PAPERS_ZIP).is_file()
    publications_url_is_not_set = (content_settings.publications_bibtex_url == '')

    return render_template('settings/content.html', **locals())


def _reset_datetime_microseconds_and_utc_offset(p_datetime):
    if p_datetime is None:
        return p_datetime

    TIME_OFFSET = 0
    if(p_datetime.utcoffset()):
        TIME_OFFSET = p_datetime.utcoffset()

    return datetime(p_datetime.year, p_datetime.month, p_datetime.day, p_datetime.hour+TIME_OFFSET, p_datetime.minute, p_datetime.second, 0)


@app.route('/settings/content/turn_on_download_default_papers')
@admin_required
def turn_on_download_default_papers():
    ContentSettings.set_download_default_papers(True)
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/turn_off_download_default_papers')
@admin_required
def turn_off_download_default_papers():
    ContentSettings.set_download_default_papers(False)
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/turn_on_prepare_downloadable_files')
@admin_required
def turn_on_prepare_downloadable_files():
    ContentSettings.set_prepare_downloadable_files(True)
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/turn_off_prepare_downloadable_files')
@admin_required
def turn_off_prepare_downloadable_files():
    ContentSettings.set_prepare_downloadable_files(False)
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/resume_all_update_jobs')
@admin_required
def resume_all_update_jobs():
    resume_neem_overview_job()
    resume_publications_job()
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/resume_neem_overview_job')
@admin_required
def route_resume_neem_overview_job():
    resume_neem_overview_job()
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/resume_publications_job')
@admin_required
def route_resume_publications_job():
    resume_publications_job()
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/pause_all_update_jobs')
@admin_required
def pause_all_update_jobs():
    pause_neem_overview_job()
    pause_publications_job()
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/pause_neem_overview_job')
@admin_required
def route_pause_neem_overview_job():
    pause_neem_overview_job()
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/pause_publications_job')
@admin_required
def route_pause_publications_job():
    pause_publications_job()
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/load_developer_settings')
@admin_required
def load_developer_settings():
    ContentSettings.set_debug_settings()
    pause_neem_overview_job()
    pause_publications_job()
    start_thread(manually_load_resource(load_default_overview_files))
    start_thread(manually_load_resource(load_default_publications_and_papers))
    return redirect(url_for('render_content_settings'))


def manually_load_resource(func):
    try:
        func()
    except Exception as e:
        app.logger.warning(e.__str__())
        flash('Action failed!', 'warning')
    else:
        flash('Action succeeded!')
        flash('Action succeeded!', 'success')


@app.route('/settings/content/update_all')
@admin_required
def manually_load_all_content_updates():
    start_thread(manually_load_resource(manual_update_neem_overview_files))
    start_thread(manually_load_resource(manual_update_publications_and_papers))
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/load_all_default')
@admin_required
def manually_load_all_default_content():
    start_thread(manually_load_resource(load_default_overview_files))
    start_thread(manually_load_resource(load_default_publications_and_papers))
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/update_publications')
@admin_required
def manually_load_publications_updates():
    start_thread(manually_load_resource(manual_update_publications_and_papers))
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/load_default_publications')
@admin_required
def manually_load_publications_defaults():
    start_thread(manually_load_resource(load_default_publications_and_papers))
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/update_overview_files')
@admin_required
def manually_load_overview_updates():
    start_thread(manually_load_resource(manual_update_neem_overview_files))
    return redirect(url_for('render_content_settings'))


@app.route('/settings/content/load_overview_default')
@admin_required
def manually_load_overview_defaults():
    start_thread(manually_load_resource(load_default_overview_files))
    return redirect(url_for('render_content_settings'))


def _send_file_if_available(FILE_PATH):
    if not Path(FILE_PATH).is_file():
        flash('Could not retrieve requested file. Check app-settings or try again later.', 'warning')
        return redirect(url_for('render_content_settings'))

    return send_file(FILE_PATH, as_attachment=True)


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
    return _send_file_if_available(DOWNLOADS_DIR_PUBLICATIONS_AND_PAPERS_ZIP)


@app.route('/settings/content/store_publications_and_papers_urls', methods=["POST"])
@admin_required
def store_publications_and_papers_urls():
    req = request.form
    if req is None:
        flash('Null request is submitted while form submission!', 'warning')
        redirect(url_for('render_content_settings'))

    ContentSettings.set_publications_bibtex_url(req.get('bibtex_url'))
    ContentSettings.set_default_papers_zip_url(req.get('default_papers_zip_url'))
    ContentSettings.set_papers_zip_url(req.get('papers_zip_url'))
    
    app.logger.info("Content settings have been updated")
    flash('Urls for papers and publications have been stored!', 'success')
    return redirect(url_for('render_content_settings'))
