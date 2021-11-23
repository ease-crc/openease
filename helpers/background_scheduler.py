import atexit

from datetime import date, datetime
from apscheduler.schedulers.background import BackgroundScheduler

from pages.publications import download_and_update_papers_and_bibtex
from pages.neem_overview import download_neem_files

OVERVIEW_SCHEDULER_JOB_ID = 'overview'
PUBLICATIONS_SCHEDULER_JOB_ID = 'publications'

BACKGROUND_SCHEDULER = BackgroundScheduler()

def start_background_scheduler():
    # ensures there is always only one BackgrounScheduler running
    if background_scheduler_is_running():
        app.logger.info('The scheduler is already active. Will not start new one.')
        return
    
    BACKGROUND_SCHEDULER.add_job(func=download_neem_files, trigger="interval", hours=3, coalesce=True, id=OVERVIEW_SCHEDULER_JOB_ID)
    BACKGROUND_SCHEDULER.add_job(func=download_and_update_papers_and_bibtex, trigger="interval", days=1, next_run_time=_get_tomorrow_3_am_datetime(), coalesce=True, id=PUBLICATIONS_SCHEDULER_JOB_ID)
    BACKGROUND_SCHEDULER.start()

    # Shut down the scheduler when exiting the app
    atexit.register(lambda: BACKGROUND_SCHEDULER.shutdown())


def _get_tomorrow_3_am_datetime():
    today = date.today()
    return datetime(today.year, today.month, today.day, 3, 0, 0)


def background_scheduler_is_running():
    return BACKGROUND_SCHEDULER.running
