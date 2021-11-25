import atexit

from datetime import date, datetime
from apscheduler.schedulers.background import BackgroundScheduler

from app_and_db import app
from pages.publications import download_and_update_papers_and_bibtex
from pages.neem_overview import download_neem_files
from postgres.settings import ContentSettings, UpdateMethod, UpdateState

OVERVIEW_SCHEDULER_JOB_ID = 'overview'
PUBLICATIONS_SCHEDULER_JOB_ID = 'publications'

BACKGROUND_SCHEDULER = BackgroundScheduler()

def start_background_scheduler():
    # ensures there is always only one BackgrounScheduler running
    if background_scheduler_is_running():
        app.logger.info('The scheduler is already active. Will not start new one.')
        return
    
    BACKGROUND_SCHEDULER.add_job(func=_update_neem_overview_files_job, trigger="interval", hours=3, coalesce=True, id=OVERVIEW_SCHEDULER_JOB_ID)
    BACKGROUND_SCHEDULER.add_job(func=_update_publications_and_papers_job, trigger="interval", days=1, next_run_time=_get_tomorrow_3_am_datetime(), coalesce=True, id=PUBLICATIONS_SCHEDULER_JOB_ID)
    BACKGROUND_SCHEDULER.start()

    _pause_update_jobs_if_necessary()

    # Shut down the scheduler when exiting the app
    atexit.register(lambda: BACKGROUND_SCHEDULER.shutdown())


def _update_neem_overview_files_job():
    download_neem_files()()
    ContentSettings.set_last_update_type_neem_overview(UpdateMethod.AUTOMATIC)


def _update_publications_and_papers_job():
    download_and_update_papers_and_bibtex()
    ContentSettings.set_last_update_publications_and_papers(UpdateMethod.AUTOMATIC)


def _get_tomorrow_3_am_datetime():
    today = date.today()
    return datetime(today.year, today.month, today.day, 3, 0, 0)


def _pause_update_jobs_if_necessary():
    """ Since the background scheduler is started even when in debug /
    developer mode, it is necessary to check whether the update jobs
    need to be active or not.
    
    This way it is ensures that the jobs already exist and only need
    to be paused or resumed later on, in case settings are changed."""
    
    content_settings = ContentSettings.get_settings()
    
    if content_settings.update_state_neem_overview == UpdateState.PAUSED:
        pause_neem_overview_job()
    
    if not content_settings.update_state_publications_and_papers == UpdateState.PAUSED:
        pause_publications_job()


def background_scheduler_is_running():
    return BACKGROUND_SCHEDULER.running


def pause_neem_overview_job():
    _pause_job(OVERVIEW_SCHEDULER_JOB_ID)
    ContentSettings.set_update_state_neem_overview(UpdateState.PAUSED)


def resume_neem_overview_job():
    _resume_job(OVERVIEW_SCHEDULER_JOB_ID)
    ContentSettings.set_update_state_neem_overview(UpdateState.ACTIVE)


def pause_publications_job():
    _pause_job(PUBLICATIONS_SCHEDULER_JOB_ID)
    ContentSettings.set_update_state_publications_and_papers(UpdateState.PAUSED)


def resume_publications_job():
    _resume_job(PUBLICATIONS_SCHEDULER_JOB_ID)
    ContentSettings.set_update_state_publications_and_papers(UpdateState.ACTIVE)


def _pause_job(job_id):
    BACKGROUND_SCHEDULER.pause_job(job_id)


def _resume_job(job_id):
    BACKGROUND_SCHEDULER.resume_job(job_id)


def get_neem_overview_job_next_runtime():
    return _get_job_next_runtime(OVERVIEW_SCHEDULER_JOB_ID)


def get_publications_job_next_runtime():
    return _get_job_next_runtime(PUBLICATIONS_SCHEDULER_JOB_ID)


def _get_job_next_runtime(job_id):
    return BACKGROUND_SCHEDULER.get_job(job_id).next_run_time
