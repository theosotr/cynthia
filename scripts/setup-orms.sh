#! /bin/bash

workdir=$1
version_file=$2
patch_dir=$HOME/patches


get_version()
{
  orm_name=$1
  version=$(grep "$orm_name: .*" < $version_file |
            sed -r "s/$orm_name: (.*)/\1/g")
  if [ -z $version ]; then
    echo "latest"
  else
    echo $version
  fi
}


if [[ ! -f "$version_file" || -z $version_file ]]; then
  django_version="latest"
  peewee_version="latest"
  sqlalchemy_version="latest"
else
  django_version=$(get_version "django")
  peewee_version=$(get_version "peewee")
  sqlalchemy_version=$(get_version "sqlalchemy")
fi


install_py_package()
{
  base=$(echo "$1" | sed -r "s/.+\/github.com\/.+\/(.+)/\1/")
  version=$2

  echo "Installing $base, version $version..."

  source $workdir/.env/bin/activate
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
  rm -rf $workdir/$base
  cd ..
}


setup()
{
  # Setup a Python virtualenv
  rm -rf $workdir/.env
  virtualenv -p python3 $workdir/.env
  source $workdir/.env/bin/activate

  # Install Python drivers and other
  pip install mysqlclient psycopg2 sqlacodegen pyodbc django-mssql-backend

  # Get the master version of Python ORMs from Github.
  install_py_package https://github.com/django/django $django_version
  install_py_package https://github.com/sqlalchemy/sqlalchemy $sqlalchemy_version
  install_py_package https://github.com/coleifer/peewee $peewee_version
}

setup
