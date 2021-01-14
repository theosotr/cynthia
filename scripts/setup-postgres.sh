#! /bin/bash

sudo apt update
sudo DEBIAN_FRONTEND=noninteractive apt-get install postgresql postgresql-contrib -yq
sudo -u postgres /usr/lib/postgresql/10/bin/pg_ctl \
  -D /etc/postgresql/10/main/ -l /var/lib/postgresql/logfile start
sudo -u postgres createuser orm_testing
sudo -u postgres psql -c "alter user orm_testing with encrypted password 'orm_testing';"
sudo -u postgres psql -c "alter user orm_testing with superuser;"
sudo sed -i -r 's/all([ ]*)peer/all\1md5/g' /etc/postgresql/10/main/pg_hba.conf
sudo /etc/init.d/postgresql restart
