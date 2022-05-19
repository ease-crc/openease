from functools import wraps
from threading import Lock, Thread
from concurrent.futures import ThreadPoolExecutor

THREAD_POOL_EXECUTOR = ThreadPoolExecutor(max_workers=3)

def start_thread(target_func):
    THREAD_POOL_EXECUTOR.submit(target_func)


def mutex_lock(mutex=None):
    """ decorator function that provides a mutex-lock
    
    If no mutex lock is assigned, a per-function mutex is used.
    
    The lock is thread-blocking, so in order to not block
    the current thread create a new one to run the desired
    function. You can do that, for example, with "start_thread" """
    
    def wrap_inner(func):
        p_mutex = _determine_mutex(mutex, func)

        @wraps(func)
        def inner(*args, **kwargs):
            p_mutex.acquire()
            try:
                func(*args, **kwargs)
            finally:
                p_mutex.release()

        return inner
    return wrap_inner


def _determine_mutex(mutex, func):
    if mutex is None:
        func.lock = Lock()
        return func.lock
    else:
        return mutex
