openEASE
================

**CAUTION**: openEASE depends on data storage infrastructure *NEEMHub*.
You will need to provide credentials for a NEEMHub server when accessing
the openEASE website for the first time.
Unfortunately, there is no documentation yet on how to set up our own NEEMHub,
but we hope we can provide a set-up tutorial soon.

## Getting Started

These instructions will get you a copy of openEASE
up and running on your local machine.

### Prerequisites

- docker

### Installation

openEASE runs in a docker environment.
This repository hosts the code for the docker image `openease/app`
which runs a flask application to serve HTML pages via HTTP.
To build the image locally, you can run in the root dir of this repository:

```Bash
docker build -t openease/app .
```

This will download all dependency for the app and create a local docker
image `openease/app`.

### Launching

openEASE uses `docker-compose` to bring up a set of docker containers and
link them with each other.
Any image that is not available locally will be downloaded automatically.
To bring up the openEASE docker containers, run this:

```Bash
docker-compose up
```

### Troubleshooting
**KnowRob** container is created when user is loggedIn. Incase of connection can not be established to the KnowRob container, one needs to take a look at knowrob container logs.

```Bash
docker logs `username`_knowrob
```

If above command fails then it is most likely that the KnowRob container is not created or failed. In this case one needs to take a look at dockerbridge.

```Bash
docker logs dockerbridge
```

#### Other docker problems
In some situations it may help to start again with a clean docker installation. This requires removing all containers and images. You can do that using following commands:


```Bash
docker rm $(docker ps -a -q)
docker rmi $(docker images -q)
```

## Content Pages

### Neem-Overview Pages

The homepage features certain neems and allows direct access to their knowledge base or overview page.
The overview page just renders the README markdown file of the neem's repository.

The homepage shows two fixed neems at the top, and below that the six most recently updated neems (based on the last commit to their respective repositories).

The server fetches the READMEs every three hours.

#### Information for NeemGit Maintainers

The README should at least contain the following:

- title
- author of the neem
- general description
- acknowledgement for fundings etc.

Furthermore, here are some suggestions for more things to include:

- details about the data or dataset used
- how to access the data
- license
- references

Check out existing READMEs or overview pages for reference or structure.
Make sure to keep the READMEs up-to-date.
Here are a few more points to consider:

- Maintainers need to have a markdown file named 'README.md' in their repositories or their neem cannot be featured on the homepage (they will still appear on the neemhub page).
The markdown should present information about the neem.
- Relevant publications need to be listed in said README.
Currently they (unfortunately) cannot be linked automatically to the neem.
- If a unordered list is not rendered properly, make sure it has a leading and trailing empty line in the README:
    ``` markdown
    some text

    - a
    - b
    - c

    more text
    ```
- Most elements of the README files should be rendered correctly.
It might happen that some elements will not pass the renderer because of the HTML-sanitizer.
If that happens, those element tags need to be added to the list of allowed tags in `get_sanitizer()` (s. `/pages/neem_overview.py`).
It might also be necessary to adjust css styling in `/static/css/overview.scss`.

#### Technical Details

For updates, the server will first fetch the list of neems for the neemhub.
Next it will check the repository of each of these neems for a README.
If one can be found, it will be downloaded and stored locally.
The repositories of each neem is then checked for the time of the last update.
Neems are then sorted by this timestamp, in order to select the six most recent neems to display on the homepage.
The featured neems at the top of the homepage are identified by their neem-id. Meanwhile information about the neem and location of the related files will be stored in the postgres database.

During an update, previous information will be overwritten (except for default files, which are kept separately).


### Publications Pages

The publications page features papers related to the EASE project.
The page includes the information of the bibtex entry for each paper and, if available, also the paper itself.

The server fetches the bibtex-files and papers (pdfs compiled into a zip-file) once a day.

#### Information for BibTEX-Entries of Publications

Make sure to follow regular guidelines for the type of publication (such as IEEE, etc.).

A few points of attention nonetheless:

- **Syntax**
    
    Check the syntax of the entry to be correct. The parser for the `bibtex`-files, `pybtex`, sadly is very unforgiving (s. [Possible Errors when Parsing bibtex-files](#possible-errors-when-parsing-bibtex-files)).
    [TEXfaq](https://texfaq.org/) is a valuable resource.

- **Authors**

    One common error is the faulty listing of authors.
    Author names need to be listed in either of the following forms:
    
    - First Last
    - Last, First
    - Last, Suffix, First

    Furthermore they need to be separated with `and`. An example for a correct listing:
    
    ``` latex
    AUTHOR = {Jamie Blond and Peter Brooks and Michael Blue}
    ```
    
    Check out [this page](https://texfaq.org/FAQ-manyauthor) for more information.

- **Title Capitalization**

    Unfortunately titles of publications, journals, etc. need to manually be set in the `bibtex`-entry in order to be properly displayed on the website.
    Python 2.7 itself only offers very rudimentary methods for capitalization of titles, and libraries such as [titlecase](https://pypi.org/project/titlecase/) do not support Python 2.7.

    Generally, letters, words, or phrases encased with curly brackets `{}` will be displayed as written (in terms of capitalizaton):

    ``` latex
    title = {A {Study} about {Smart Robots}}
    ``` 

    You can read more about this topic [here](https://texfaq.org/FAQ-capbibtex).

#### Possible Errors when Parsing bibtex-files

The server uses [`pybtex`](https://pybtex.org/) for parsing the bibtex-files.
Said library is very sensitive when it comes to the syntax and will fail to parse even on a single error.
Other parsers had different problems, overall `pybtex` was still the preferred choice.

You can use the [following](https://github.com/navidJadid/publications-bibtex-tester) tool to check if a given bibtex-file would cause errors when parsed by the server.

[TEXfaq](https://texfaq.org/) also offers answers to commonly asked questions about this topic.

#### Technical Details

For updates, first an url for both the `publications.bib` and `papers.zip` need to be provided (s. [Content Settings page](#content-settings-page)).
The former contains information about all EASE related publications. Only a subset of those will be displayed on the publications page later.
To be specific, those which have the following keywords in their `bibtex`-entries:

- openease_overview
- openease_kb_of_exp_data
- openease_cram
- openease_knowledge_representation
- openease_perception
- openease_human_activity
- openease_manipulation
- openease_natural_language

The `bib`-file will be parsed and stored in a python dictionary.
Also for each entry, the program checks if a paper is available as a pdf (from `papers.zip`) and links it, if available.

During an update, previous information will be overwritten (except for default files, which are kept separately).


### News Blog

The news blog uses an external service: a [directus](https://directus.io/) cms (content management system). Articles are written on that service and accessed from the webserver backend. It is necessary to provide a url to the CMS in the content settings (s. [Content-Settings Page](#content-settings-page)).

The current CMS is hosted at (TODO).

#### CMS Requirements

The CMS needs a collection called 'articles' which should be publically accessible. The collection needs to be configured to possess at least the following fields (with the specified type):

- id (counting integer - primary key)
- title (string)
- text (WYSIWYG)
- author (user)
    - first_name (string)
    - last_name (string)
- publication date (date)
- sort (sort)

The former 4 fields and the author name fields need to be configured to be publically accessible (technically all other fields can be hidden from the public). 

**Note**: The official openEASE CMS supports login via neemGit, thus any neemgit member can write articles. Additionally the admin can manually create and distribute accounts.

#### CMS API Access 101

If the CMS is properly configured, articles can be accessed as follows:

- single articles: `<news-cms base-url> /items/articles/ <article id> ? <query parameters>`
- multiple articles: `<news-cms base-url> /items/articles ? <query parameters>`

Here are a few useful query parameters

- `fields=*, author.*`

    retrieve all available fields for the articles and respective authors <br>
    (this is necessary, because otherwise the author name fields would not get fetched)

- `sort=sort,-publication_date`

    sort by publication date latest to oldest

- `limit=<number>`
    
    retrieve requested amount of entries

- `page=<number>`

    retrieve items from page x; calculated with limit

- `search=<query>`

    searches articles with given queries

- `meta=*`

    retrieve collection meta data, like total and filtered number of articles


An example query could look like this:

```
/items/articles?fields=*, author.*&sort=sort,-publication_date&limit=3
```

Which would fetch the three latest articles with all their exposed fields.

For more details on how the CMS and its API work check out the [openEASE cms repository](https://github.com/ease-crc/openease_cms) and official [directus docs](https://docs.directus.io/reference/introduction.html).

#### Development with Local CMS

If you want to host your own CMS locally for e.g. development or testing, follow these steps:

1. Create a new docker network called `cms-development-network`:

    ```
    docker network create cms-development-network
    ```

1. Add the following lines to the docker-compose files of openEASE and your directus CMS:

    general network section
    ```
    networks:
      [...]
      # only needed if running both openease and cms locally
      cms-development-network:
          external: true
    ```

    network section of `openease` and `directus` service
    ```
    networks:
      - [...]
      # only needed if running both openease and cms locally
      - cms-development-network
    ```

    **Note**: If you are using the official openEASE repositories, these lines should already be in the compose files. Thus, it is only necessary to uncomment them.

1. In the openease admin content panel set the news cms url to:

    ```
    http://<name of directus container/service>:8055
    ```

Now the two services should be able to connect on your local machine.


### Content-Settings Page

The content-settings page shows the state of all the content for the neem-overview and publications pages, which includes:

- state of content loaded (none, default, latest)
- state of the update jobs (active, paused)
- time of next update
- time of previous update
- type of previous update (automatic, manual)

In addition, the following action can be performed:

- manually load updates for the content
- pause and resume update jobs for the content
- load default files for any given content type
- turn on/off to download 'default_papers.zip`
- turn on/off to prepare downloadable files for the admin (these include the papers, markdowns, python dictionaries for neem-overview or publications data as JSON)
- set developer settings (= deactivate unneeded functionalities)
- download 'content files' (s. [How to Update Default Files](#how-to-update-default-files))
- set the url to an external News CMS (for details s. [News Blog](#news-blog))

Lastly, the content-settings page is where the url or local path for the publications `bibtex` and `papers.zip` need to be set.
If using the local path, provide the relative paths to `/content/publications-and-papers/` in the settings panel and place the mentioned files there.

At least the `bibtex` file needs to be provided, otherwise the publications files cannot be updated.
Urls need to start with 'http(s)://'.

Each function is provided with a tooltip to explain it's function.

#### Developer Settings

The server will load debug settings for the content, if the environment variable 'EASE_DEBUG' is set to 'true'.
That includes:

- upon start-up, either previously loaded content or default content is loaded (=faster start-up); no updates for neem-overview and publications are fetched
- update jobs are set to paused
- 'download default papers' is set to 'OFF'
- 'prepare downloadable files' is set to 'OFF'

In DEBUG mode, changes to the settings persist even after the lifetime of the container.
This is useful if certain behaviours need to be examined upon restarting or rebuilding the container (otherwise these settings would need to be manually set each time).
If you made changed settings previously but want the stock developer settings again, just click 'load developer settings' on the content-settings page.

#### Production Settings

If the environment variable 'EASE_DEBUG' is set to 'false', the server will load production settings for the content.
That includes:

- upon start-up, updates for neem-overview and publications are fetched
- update jobs are set to active
- 'download default papers' is set to 'ON'
- 'prepare downloadable files' is set to 'ON'

#### How to Update Default Files

In order to update default files for content (for example, to include newer neem overviews or publications) fetch the latest updates on a running server in the admin content panel, while having checked the `prepare downloadable files` option.
Next, download the files of choice and replace the appropriate files in this repository in `/content/default_files`.
