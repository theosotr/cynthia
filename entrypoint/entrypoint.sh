#! /bin/bash

set +e

# Initialize Backends
echo "Initialize Backends"

source $HOME/.env/bin/activate
nohup sudo /opt/mssql/bin/sqlservr start &
sudo /usr/bin/mysqld_safe >/dev/null 2>&1 &
timeout=30
while ! /usr/bin/mysqladmin -u root status >/dev/null 2>&1
do
  timeout=$(($timeout - 1))
  if [ $timeout -eq 0 ]; then
    echo -e "\nCould not connect to database server. Aborting..."
    exit 1
  fi
  sleep 1
done
sudo /etc/init.d/postgresql start


source $HOME/.env/bin/activate
if [ $# -eq 0  ]; then
  /bin/bash
  exit 0
fi
echo
echo
echo "Running $@"
eval "$@"
