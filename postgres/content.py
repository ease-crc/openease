from app_and_db import app, db
from datetime import datetime

DATETIME_MIN = datetime.min

keywords_association_table = db.Table('keywords_association',
    db.Column('publications_id', db.Integer, db.ForeignKey('publications_data.id', ondelete="CASCADE"), primary_key=True),
    db.Column('keyword_id', db.Integer, db.ForeignKey('publications_keywords.id', ondelete="CASCADE"), primary_key=True)
)

class PublicationsData(db.Model):
    """
    DB model class for storing publications data
    """
    id = db.Column(db.Integer, primary_key=True)
    key = db.Column(db.String(255), nullable=False, default='')
    title = db.Column(db.String(255), nullable=False, default='')
    authors = db.Column(db.String(511), nullable=False, default='')
    year = db.Column(db.Integer(), nullable=False, default='')
    abstract = db.Column(db.String(2000), nullable=False, default='')
    has_pdf = db.Column(db.Boolean(), nullable=False, default=False)
    doi = db.Column(db.String(255), nullable=True, default='')
    url = db.Column(db.String(255), nullable=True, default='')
    bibtex_str = db.Column(db.String(3000), nullable=False, default='')
    bibtex_html_str = db.Column(db.String(3000), nullable=False, default='')
    html_str = db.Column(db.String(2000), nullable=False, default='')
    reference_str = db.Column(db.String(2000), nullable=False, default='')
    keywords = db.relationship(
        "PublicationsKeywords",
        secondary = keywords_association_table,
        back_populates = 'publications',
        passive_deletes=True
    ) 


class PublicationsKeywords(db.Model):
    """
    DB model class for storing publications keywords
    """
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(255), nullable=False, default='')
    title = db.Column(db.String(255), nullable=False, default='')
    publications = db.relationship(
        "PublicationsData",
        secondary = keywords_association_table,
        back_populates = 'keywords',
        cascade="all, delete"
    ) 


class NeemOverviewData(db.Model):
    """
    DB model class for storing neem overview data
    """
    id = db.Column(db.Integer, primary_key=True)
    neem_id = db.Column(db.String(255), nullable=False, default='')
    name = db.Column(db.String(255), nullable=False, default='')
    description = db.Column(db.String(511), nullable=False, default='')
    maintainer = db.Column(db.String(255), nullable=False, default='')
    downloadUrl = db.Column(db.String(255), nullable=False, default='')
    last_updated = db.Column(db.DateTime(), nullable=False, default=DATETIME_MIN)
    # recent flag only exists for faster db queries, because it saves
    # the time of sorting the whole table each request
    recent = db.Column(db.Boolean(), nullable=False, default=False)
    neem_repo_path = db.Column(db.String(255), nullable=False, default='')
    image = db.Column(db.String(255), nullable=False, default='')
    featured = db.Column(db.Boolean(), nullable=False, default=False)
