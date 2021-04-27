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

