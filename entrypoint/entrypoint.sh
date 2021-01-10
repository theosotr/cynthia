#! /bin/bash

set +e

if [ $# -eq 0  ]; then
  /bin/bash
  exit 0
fi


OPTS=`getopt --long django-version:,peewee-version:,sqlalchemy-version: \
             activerecord-version:,sequelize-version: "$@"`

if [ $? != 0 ]; then
  exit 1
fi

# Note the quotes around `$TEMP': they are essential!
eval set -- "$OPTS"

django_v=latest
peewee_v=latest
sqlalchemy_v=latest
activerecord_v=latest
sequelize_v=latest

while true; do
  case "$1" in
    --django-version )       django_v="$2"; shift 2 ;;
    --peewee-version )       peewee_v="$2"; shift 2 ;;
    --sqlalchemy-version )   sqlalchemy_v="$2"; shift 2 ;;
    --activerecord-version ) activerecord_v="$2"; shift 2 ;;
    --sequelize-version )    sequelize_v="$2"; shift 2 ;;
    -- ) shift ; break ;;
    * ) break ;;
  esac
done


echo "django: $django_v" > $HOME/versions.txt
echo "peewee: $peewee_v" >> $HOME/versions.txt
echo "sqlalchemy: $sqlalchemy_v" > $HOME/versions.txt
echo "activerecord: $activerecord_v" >> $HOME/versions.txt
echo "sequelize: $sequelize_v" >> $HOME/versions.txt

# Setup ORMs
$HOME/scripts/setup-orms.sh $HOME

# Initialize Backends
echo "Initialize Backends"

nohup sudo /opt/mssql/bin/sqlservr start &
sudo /etc/init.d/mysql start
sudo /etc/init.d/postgresql start

source $HOME/.env/bin/activate
if [ $# -eq 0  ]; then
  /bin/bash
  exit 0
fi
eval "$@"
