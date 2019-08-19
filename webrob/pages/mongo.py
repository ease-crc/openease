import webrob.utility.system_environment_variable_getter as evg

from flask import render_template, stream_with_context, Response

from subprocess import call

from webrob.pages.experiments import get_experiment_list, get_experiment_path
from webrob.utility.path_handler import join_paths, is_directory, get_path_size
from webrob.utility.directory_handler import list_directories
from webrob.utility.utility import admin_required
from webrob.app_and_db import app

from pymongo import MongoClient

__author__ = 'danielb@cs.uni-bremen.de'


@app.route('/knowrob/admin/mongo')
@admin_required
def admin_mongo():
    mongo_db = _connect_to_mongo_db()

    db_info, file_info = _get_db_and_file_info(mongo_db)

    mongo_db.close()
    return render_template('admin/mongo.html', **locals())


def _connect_to_mongo_db():
    host = evg.get_required_variable('MONGO_PORT_27017_TCP_ADDR')
    port = evg.get_required_variable('MONGO_PORT_27017_TCP_PORT')
    return MongoClient(host, int(port))


def _get_db_and_file_info(mongo_db):
    db_info = {}
    file_info = {}

    for (cat, exp) in get_experiment_list():
        db_name = _build_mongo_db_name(cat, exp)
        # Mongo stats
        db_info[db_name] = _get_db_info(db_name, mongo_db)

        file_info[db_name] = _get_file_info(cat, exp)

    return db_info, file_info


def _build_mongo_db_name(category, experiment):
    return category + "_" + experiment


def _get_db_info(db_name, mongo_db):
    stats = mongo_db[db_name].command("dbStats")
    return _get_formatted_db_stats(stats)


def _get_formatted_db_stats(stats):
    return {
        'size': round((stats['dataSize']) / (1024.0 * 1024.0), 2),
        'avgObjSize': round((stats['avgObjSize']) / (1024.0 * 1024.0), 2),
        'objects': stats['objects'],
        'collections': stats['collections']
    }


def _get_file_info(cat, exp):
    size, collections = _get_episode_sizes_and_collections(cat, exp)
    return _get_formatted_file_info(cat, collections, exp, size)


def _get_episode_sizes_and_collections(cat, exp):
    size = 0
    collections = set()
    for (collection, f) in _get_episode_files(cat, exp):
        size += round((get_path_size(f)) / (1024.0 * 1024.0), 2)
        collections.add(collection)
    return size, collections


def _get_episode_files(cat, exp):
    episode_files = []
    exp_path = get_experiment_path(cat, exp)
    for episode in list_directories(exp_path):
        episode_dir = join_paths(exp_path, episode)
        if not is_directory(episode_dir):
            continue
        for f in list_directories(episode_dir):
            if f.endswith('.json') or f.endswith('.bson'):
                episode_files.append((f[:-5], join_paths(episode_dir, f)))
    return episode_files


def _get_formatted_file_info(cat, collections, exp, size):
    return {
        'category': cat,
        'experiment': exp,
        'episodes': _get_episode_count(cat, exp),
        'size': size,
        'collections': list(collections)
    }


def _get_episode_count(cat, exp):
    exp_path = get_experiment_path(cat, exp)
    count = 0
    for episode in list_directories(exp_path):
        episode_dir = join_paths(exp_path, episode)
        if not is_directory(episode_dir):
            continue
        count += 1
    return count


@app.route('/knowrob/admin/mongo_update/<cat>/<exp>', methods=['GET', 'POST'])
@admin_required
def admin_mongo_update(cat, exp):
    db_name = _build_mongo_db_name(cat, exp)
    _drop_old_db_content(db_name)

    # TODO: check whether functionalities inside mongo_import() can be moved outside the method without error
    # Import all JSON/BSON files in episode directories
    def mongo_import():
        collections = set()
        for (collection, data_file) in _get_episode_files(cat, exp):
            app.logger.info("Importing " + data_file)
            collections.add(collection)
            if data_file.endswith('.json'):
                _mongo_import_json(db_name, collection, data_file)
            if data_file.endswith('.bson'):
                _mongo_import_bson(db_name, collection, data_file)
            yield 'Imported %s.\n' % (data_file)

        # Create indices
        mongo = _connect_to_mongo_db()
        db = mongo[db_name]
        for collection_name in list(collections):
            # TODO: make this more flexible
            db[collection_name].ensure_index('__recorded')
            yield 'Search index created for %s.__recorded.\n' % (collection_name)
            db[collection_name].ensure_index('transforms.header.stamp')
            yield 'Search index created for %s.transforms.header.stamp.\n' % (collection_name)
        mongo.close()

    return Response(stream_with_context(mongo_import()))


def _drop_old_db_content(db_name):
    mongo = _connect_to_mongo_db()
    mongo.drop_database(db_name)
    mongo.close()


def _mongo_import_json(db_name, collection, json_file):
    call(["mongoimport",
          "--host", evg.get_required_variable('MONGO_PORT_27017_TCP_ADDR'),
          "--port", evg.get_required_variable('MONGO_PORT_27017_TCP_ADDR'),
          "--db", db_name,
          "--collection", collection,
          "--file", str(json_file)
          ])


def _mongo_import_bson(db_name, collection, bson_file):
    call(["mongorestore",
          "--host", evg.get_required_variable('MONGO_PORT_27017_TCP_ADDR'),
          "--port", evg.get_required_variable('MONGO_PORT_27017_TCP_ADDR'),
          "--db", db_name,
          "--collection", collection,
          str(bson_file)
          ])
