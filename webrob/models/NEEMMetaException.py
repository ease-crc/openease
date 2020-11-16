# class is the custom NEEM meta exception handler. It basically is raised when there is problem with connecting to
# neems mongodb meta collection

class NEEMMetaException(Exception):
    # Bad Request
    status_code = 400

    def __init__(self, message, exc=None, status_code=None):
        Exception.__init__(self)
        self.message = message
        self.exc = exc
        if status_code is not None:
            self.status_code = status_code
        if exc is not None:
            self.exc = exc


    def get_exc(self):
        return self.exc

    def get_message(self):
        return self.message