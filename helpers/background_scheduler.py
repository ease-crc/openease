import atexit
import logging

from apscheduler.schedulers.background import BackgroundScheduler

from app_and_db import app
from pages.publications import automatic_update_publications_and_papers
from pages.neem_overview import automatic_update_neem_overview_files
from postgres.settings import ContentSettings, UpdateMethod, UpdateState

OVERVIEW_SCHEDULER_JOB_ID = 'overview'
PUBLICATIONS_SCHEDULER_JOB_ID = 'publications'

BACKGROUND_SCHEDULER = BackgroundScheduler()
SCHEDULER_LOGGER = logging.getLogger('apscheduler.executors.default')

def start_background_scheduler():
    _init_scheduler_logger()
    # ensures there is always only one BackgrounScheduler running
    if background_scheduler_is_running():
        app.logger.info('The scheduler is already active. Will not start new one.')
        return
    
    BACKGROUND_SCHEDULER.add_job(func=automatic_update_neem_overview_files, trigger="cron", hour='*/3', coalesce=True, id=OVERVIEW_SCHEDULER_JOB_ID)
    BACKGROUND_SCHEDULER.add_job(func=automatic_update_publications_and_papers, trigger="cron", hour='3', coalesce=True, id=PUBLICATIONS_SCHEDULER_JOB_ID)
    BACKGROUND_SCHEDULER.start()

    _pause_update_jobs_if_necessary()

    # Shut down the scheduler when exiting the app
    atexit.register(lambda: BACKGROUND_SCHEDULER.shutdown())


def _init_scheduler_logger():
    global SCHEDULER_LOGGER

    SCHEDULER_LOGGER.setLevel(logging.INFO)  # DEBUG

    fmt = logging.Formatter('%(levelname)s:%(name)s:%(message)s')
    h = logging.StreamHandler()
    h.setFormatter(fmt)
    SCHEDULER_LOGGER.addHandler(h)


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
