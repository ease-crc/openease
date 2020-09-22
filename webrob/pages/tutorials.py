import json
import re
from urlparse import urlparse

from flask import request, session, render_template, Markup, jsonify
from flask.ext.misaka import markdown
from flask_user import current_user

from webrob.app_and_db import app
from webrob.models.teaching import find_courses, get_exercises, get_task
from webrob.models.tutorials import read_tutorial_page


@app.route('/tutorials/')
def tutorials():
    error = ""
    # determine hostname/IP we are currently using
    # (needed for accessing container)
    host_url = urlparse(request.host_url).hostname
    container_name = 'tutorials'
    show_south_pane = False
    readonly = True
    authentication = False
    session['video'] = 0

    # Use the user container if user is authenticated
    if current_user.is_authenticated:
        container_name = current_user.username + "_knowrob"
        #container_name = session['user_container_name']
        authentication = True
    
   
    return render_template('pages/tutorials.html', **locals())


@app.route('/tutorials/get', methods=['POST'])
def get_tutorial():
    data = ""
    with open('/opt/webapp/webrob/tutorials/tutorial-semweb.md', 'r') as f:
        data = f.read()
    
    response = read_tutorial(data)
    # data = json.loads(request.data)
    # response = read_tutorial(data['category'], data['page'])
    if response is None:
        return jsonify({})
    else:
        return jsonify(response)


def read_tutorial(page):
    if page is None:
        app.logger.info("No tutorial available")
        return None
    content = markdown(page, fenced_code=True)

    # automatically add event handler for highlighting DOM elements
    tmp = re.findall('<em>(.*?)</em>', str(content))
    for t in tmp:
        if 'hl_' in t:
            text = t.split(' hl_')[0]
            idname = t.split(' hl_')[1]
            content = re.sub('<em>{} hl_{}</em>'.format(text, idname),
                             '<em onmouseover="highlightElement(&#39;{0}&#39;, &#39;id&#39;, true)" onmouseout="highlightElement(&#39;{0}&#39;, &#39;id&#39;, false)">{1}</em>'.format(idname, text), str(content))
        elif 'hlc_' in t:
            text = t.split(' hlc_')[0]
            classname = t.split(' hlc_')[1]
            content = re.sub('<em>{} hlc_{}</em>'.format(text, classname),
                             '<em onmouseover="highlightElement(&#39;{0}&#39;, &#39;class&#39;, true)" onmouseout="highlightElement(&#39;{0}&#39;, &#39;class&#39;, false)">{1}</em>'.format(classname, text), str(content))

    # automatically add "ask as query" links after code blocks
    content = re.sub('</code>(\s)?</pre>',
                     "</code></pre><div class='show_code'><a href='#' class='show_code'>Ask as query</a></div>",
                     str(content))
    content = Markup(content)
    # check whether there is another tutorial in this category
    #nxt = read_tutorial_page(cat_id, int(page) + 1)
    #prev = read_tutorial_page(cat_id, int(page) - 1)

    out = dict()
    out['this'] = {
        'cat_id': "overview",
        'page': 1,
        'title': "Semantic Web Introduction",
        'text': content
    }
    # nxt = {}
    # prev = {}
    # if nxt is not None:
    #     out['next'] = {
    #         'cat_id': nxt.cat_id,
    #         'page': nxt.page,
    #         'title': nxt.title
    #     }
    # if prev is not None:
    #     out['prev'] = {
    #         'cat_id': prev.cat_id,
    #         'page': prev.page,
    #         'title': prev.title
    #     }

    return out


@app.route('/teaching/search', methods=['POST'])
def find_course():
    data = json.loads(request.data)
    course_name = data['course']
    courses = []
    for c in find_courses(course_name):
        courses.append({
            'id': c.id,
            'name': c.name,
            'term': c.term,
            'university': c.university
        })
    return jsonify(courses)


@app.route('/teaching/get_exercises', methods=['POST'])
def get_exercise_():
    data = json.loads(request.data)
    course_id = data['course_id']
    exercises = []
    for e in get_exercises(course_id):
        exercises.append({
            'id': e.id,
            'course_id': e.course_id,
            'number': e.number,
            'title': e.title
        })
    return jsonify(exercises)


@app.route('/teaching/get_task', methods=['POST'])
def get_task_():
    data = json.loads(request.data)
    exercise_id = data['exercise_id']
    task_number = data['task_number']

    task = get_task(exercise_id, task_number)
    if task is None:
        return jsonify(None)
    content = markdown(task.text, fenced_code=True)

    # automatically add "ask as query" links after code blocks
    content = re.sub('</code>(\s)?</pre>',
                     "</code></pre><div class='show_code'><a href='#' class='show_code'>Ask as query</a></div>",
                     str(content))
    content = Markup(content)
    # check whether there is another task in this exercise
    nxt = get_task(exercise_id, task_number + 1)
    prev = get_task(exercise_id, task_number - 1)

    out = dict()
    out['this'] = {
        'exercise_id': task.exercise_id,
        'number': task.number,
        'title': task.title,
        'text': content
    }
    if nxt is not None:
        out['next'] = {
            'exercise_id': nxt.exercise_id,
            'number': nxt.number,
            'title': nxt.title
        }
    if prev is not None:
        out['prev'] = {
            'exercise_id': prev.exercise_id,
            'number': prev.number,
            'title': prev.title
        }

    return jsonify(out)


@app.route('/teaching/')
def teaching():
    error = ""
    # determine hostname/IP we are currently using
    # (needed for accessing container)
    host_url = urlparse(request.host_url).hostname
    container_name = session['user_container_name']
    show_south_pane = False
    # readonly = True
    # authentication = False
    session['video'] = 0

    return render_template('knowrob_teaching.html', **locals())
