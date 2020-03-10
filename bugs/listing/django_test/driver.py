import os, django
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "mysite.settings")
django.setup()


from listing.models import Listings

q1 = Listings.objects.all()
q2 = Listings.objects.all()
q3 = q1.union(q2)

for element in q3.order_by('yearly_rent'):
    print (element.yearly_rent)
