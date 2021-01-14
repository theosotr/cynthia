#! /bin/bash


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

workdir=$HOME
patch_dir=$HOME/patches


install_py_package()
{
  base=$(echo "$1" | sed -r "s/.+\/github.com\/.+\/(.+)/\1/")
  version=$2

  echo "Installing $base, version $version..."

  cd $HOME/$base
  git checkout .
  git pull origin
  if [ "$version" != "latest" ]; then
    git checkout $version
  fi

  if [ "$base" = "peewee" ]; then
    # Apply patches to the peewee ORM.
    patch < $patch_dir/peewee.patch
    patch < $patch_dir/pwiz.patch
  fi
  pip uninstall $base -y
  python setup.py install
  cd ..
}


setup()
{
  # Get the master version of Python ORMs from Github.
  if [ ! -z $django_v ]; then
    install_py_package https://github.com/django/django $django_v
  fi

  if [ ! -z $sqlalchemy_v ]; then
    install_py_package https://github.com/sqlalchemy/sqlalchemy $sqlalchemy_v
  fi

  if [ ! -z $peewee_v ]; then
    install_py_package https://github.com/coleifer/peewee $peewee_v
  fi
}

setup
