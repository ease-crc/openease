import ast
import re
import requests
from datetime import datetime

from flask import redirect, flash, render_template, url_for, request
from flask_paginate import Pagination

from app_and_db import app
from helpers.utility import sanitize_html
from postgres.settings import ContentSettings

@app.route('/news')
def render_news():
    # retrieve search query, if any
    query = request.args.get('news_query', default='', type=str)

    # compute pagination offset
    limit_per_page = 5
    current_page = int(request.args.get('page', default=1))

    # get news articles from cms
    cms_data = get_newspage_articles(limit_per_page, current_page, search_query=query)
    meta_data = cms_data['meta']
    news_articles = cms_data['data']
    display_count = meta_data['filter_count'] if query else meta_data['total_count']

    pagination = Pagination(page=current_page,
                            per_page=limit_per_page,
                            total=display_count,
                            css_framework='bootstrap4')

    return render_template('pages/news.html', **locals())


@app.route('/news_article/<id>')
def render_news_article(id=None):
    """ When tags or items from the markdown are not displayed correctly,
    it might hint to the sanitizer removing unallowed tags. To allow 
    these tags to pass, adjust the sanitizer-config from get_sanitizer()
    in # pages/overview.py. Afterwards adjust the styling in 
    static/css/overview.scss.
    
    When in doubt, refer to
      https://github.com/trentm/python-markdown2
    and
      https://github.com/matthiask/html-sanitizer """
    
    news_cms_base_url = _get_news_cms_base_url()
    if not news_cms_base_url:
        _flash_problem_with_news()
        return redirect(url_for('render_homepage'))
    
    url = (news_cms_base_url            +   # base url
          '/items/articles/'            +   # retrieve items from the articles collection
          id                            +   # article id
          '?fields=*, author.*')            # retrieve all available fields for the articles and respective authors
    
    try:
        r = requests.get(url, allow_redirects=True)
    except Exception as e:
        _log_sending_request_had_issues(e.__str__())
        _flash_problem_with_news()
        return redirect(url_for('render_homepage'))

    if not _response_status_is_okay(r.status_code):
        _log_error_response(str(r.status_code), r.text)
        _flash_problem_with_news()
        return redirect(url_for('render_homepage'))
    
    article = _prepare_news_article_from_response(ast.literal_eval(r.content.decode('UTF-8')))
    
    return render_template('pages/news-article.html', **locals())


def _response_status_is_okay(response_status_code):
    return (response_status_code == 200)


def _prepare_news_article_from_response(response):
    article = response['data']
    article['text'] = sanitize_html(article['text'])
    article['publication_date'] = _fix_datetime_str(article['publication_date'])
    return article


def _fix_datetime_str(cms_datetime):
    dt = datetime.strptime(cms_datetime, '%Y-%m-%dT%H:%M:%S')
    return dt.strftime('%b-%d-%Y %H:%M')


def _log_news_cms_url_not_set():
    app.logger.info('News CMS Url is not set, therefore cannot fetch news articles.')


def _log_sending_request_had_issues(error_message):
    app.logger.info('Sending the request for the news articles had some issues.\n\n' + error_message)


def _log_error_response(status_code, text):
    app.logger.info('News CMS response code: ' + status_code)
    app.logger.info('News CMS response message: ' + text)


def _flash_problem_with_news():
    flash('We are currently experiencing issues with the news pages.\n\
        Please try again later.')


def get_newspage_articles(limit, page, search_query=''):
    url = ('/items/articles'                 # retrieve items from the articles collection
           '?fields=*, author.*'             # retrieve all available fields for the articles and respective authors
           '&sort=sort,-publication_date'    # sort by publication date latest to oldest
           '&limit='   + str(limit)     +    # retrieve requested amount of entries
           '&page='    + str(page)      +    # retrieve items from page x; calculated with limit
           '&search='  + search_query   +    # search query if available
           '&meta=*')
    
    return _get_news_articles_from_cms(url)


def get_homepage_news_articles():
    url = ('/items/articles'                 # retrieve items from the articles collection
           '?fields=*, author.*'             # retrieve all available fields for the articles and respective authors
           '&sort=sort,-publication_date'    # sort by publication date latest to oldest
           '&limit=3')                       # retrieve 3 entries
    
    return _get_news_articles_from_cms(url)


def _get_news_articles_from_cms(url_arguments):
    news_cms_base_url = _get_news_cms_base_url()
    if not news_cms_base_url:
        return {}
    
    return _get_and_prepare_news_articles_from_url(news_cms_base_url + url_arguments)


def _get_news_cms_base_url():
    content_setting = ContentSettings.get_settings()
    if not content_setting.news_cms_url:
        _log_news_cms_url_not_set()
        return ""
    return content_setting.news_cms_url


def _get_and_prepare_news_articles_from_url(request_url):
    try:
        r = requests.get(request_url, allow_redirects=True)
    except Exception as e:
        _log_sending_request_had_issues(e.__str__())
        return {}
    
    if not _response_status_is_okay(r.status_code):
        return {}

    return _prepare_news_articles_from_response(ast.literal_eval(r.content.decode('UTF-8')))


def _prepare_news_articles_from_response(response_data):
    news_articles = response_data

    for entry in news_articles['data']:
        entry['text'] = sanitize_html(entry['text'])
        entry['text'] = _remove_all_tags(entry['text'])
        entry['text'] = _reduce_paragraph(entry['text'])
        entry['publication_date'] = _fix_datetime_str(entry['publication_date'])
    
    return news_articles


def _remove_all_tags(html_str):
    return re.sub('<[^<]+?>', '', html_str)


def _reduce_paragraph(text_str):
    return text_str[:220] + "..."
