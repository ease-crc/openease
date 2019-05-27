from webrob.utility.db_connection_checker import got_db_connection


def init_db(app, db):
    # Automatically create all DB tables in app/app.sqlite file
    if got_db_connection(app, db):
        db.create_all()
        db.session.commit()
