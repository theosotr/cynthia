#! /bin/bash


workdir=$(realpath $1)
patch_dir=$(realpath $2)

if [ -z $workdir ]; then
  echo "You have to provide the working directory (-w option)"
fi


if [ -z $patch_dir ]; then
  echo "You have to provide the directory of patches (-p option)"
fi

shift 2


install_py_package()
{
  base=$(echo "$1" | sed -r "s/.+\/github.com\/.+\/(.+)/\1/")
  git clone $1 $workdir/$base
  source $workdir/.env/bin/activate
  cd $workdir/$base
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
  pip install mysqlclient psycopg2 sqlacodegen

  # Get the master version of Python ORMs from Github.
  install_py_package https://github.com/django/django
  install_py_package https://github.com/sqlalchemy/sqlalchemy
  install_py_package https://github.com/coleifer/peewee

  npm install --save sequelize
  npm install --save sequelize-auto
  npm install --save sqlite3
  npm install --save pg pg-hstore
  npm install --save mysql2
}


cynthia_test()
{
  cd $workdir
  source $workdir/.env/bin/activate
  echo "Running cynthia test $@"
  cynthia test "$@"
  target="$(date +'%d-%m-%y')-testing-session"
  rm -rf $target
  mv .cynthia $target
  echo "cynthia test $@" > $target/command.txt
}

setup
cynthia_test "$@"
