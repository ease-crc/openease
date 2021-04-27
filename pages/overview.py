import json
import requests

from html_sanitizer import Sanitizer
from html_sanitizer.sanitizer import sanitize_href, bold_span_to_strong,italic_span_to_em, target_blank_noopener, tag_replacer

from neems.neemhub import instance as neemhub, NEEMHubConnectionError

from app_and_db import app

def load_markdown_content():
    app.logger.info("Downloading md-files...")

    try:
        matching_neems = neemhub.get_neem_ids('', True)
        neems = list(map(lambda (x): neemhub.get_neem(x), matching_neems))
    except Exception as e:
        app.logger.error('Could not connect to Neemhub to fetch markdowns for neems.\n\n' + e.__str__())
    else:
        for neem in neems:      # download and save md-file
            url = neem.downloadUrl + '/-/raw/master/README.md'
            r = requests.get(url, allow_redirects=True)
            if r.status_code == 200:
                with open('/opt/webapp/webrob/overview-contents/' + neem.neem_repo_path + '.md', 'wb') as md_file:
                    md_file.write(r.content)

        app.logger.info("Finished md-file downloads.")


def get_sanitizer():
    # When tags or items from the markdown are not displayed correctly,
    # it might hint to the sanitizer removing unallowed tags. To allow 
    # these tags to pass, adjust the sanitizer-config from get_sanitizer()
    # in # pages/overview.py. Afterwards adjust the styling in 
    # static/css/overview.scss.
    #
    # When in doubt, refer to
    #   https://github.com/trentm/python-markdown2
    # and
    #   https://github.com/matthiask/html-sanitizer
    
    return Sanitizer({
        "tags": {
            "a", "h1", "h2", "h3", "strong", "em", "p", "ul", "ol",
            "li", "br", "sub", "sup", "hr", "img", "blockquote",
            "table", "thead", "tbody", "tr", "th", "td",
        },
        "attributes": {
            "a": ("href", "name", "target", "title", "id", "rel"),
            "img": ("src", "alt", "width", "height"),
        },
        "empty": {"hr", "a", "br", "img", "tr", "th", "td"},
        "separate": {
            "a", "p", "li", "img", "table", "tr", "th", "td", "blockquote",
        },
        "whitespace": {"br"},
        "keep_typographic_whitespace": False,
        "add_nofollow": False,
        "autolink": False,
        "sanitize_href": sanitize_href,
        "element_preprocessors": [
            # convert span elements into em/strong if a matching style rule
            # has been found. strong has precedence, strong & em at the same
            # time is not supported
            bold_span_to_strong,
            italic_span_to_em,
            tag_replacer("b", "strong"),
            tag_replacer("i", "em"),
            tag_replacer("form", "p"),
            target_blank_noopener,
        ],
        "element_postprocessors": [],
        "is_mergeable": lambda e1, e2: True
    })
