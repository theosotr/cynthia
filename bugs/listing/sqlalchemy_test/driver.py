from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from models import Listing


engine = create_engine('postgresql://user:user@localhost:5432/user')
Session = sessionmaker(bind=engine)
session = Session()

q1 = session.query(Listing).filter()
q2 = session.query(Listing).filter()
q3 = q1.union(q2)

for element in q3.order_by(Listing.yearly_rent).all():
    print (element.yearly_rent)
