# coding: utf-8
from sqlalchemy import Boolean, Column, Integer, Numeric, text
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()
metadata = Base.metadata


class Listing(Base):
    __tablename__ = 'listings'

    id = Column(Integer, primary_key=True, server_default=text("nextval('listings_id_seq'::regclass)"))
    sale_price = Column(Numeric(10, 0), nullable=False)
    yearly_rent = Column(Numeric(10, 0), nullable=False)
    toto = Column(Boolean, nullable=False)
