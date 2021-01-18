#! /bin/bash

set +e

# Initialize Backends
echo "Initialize Backends"

source $HOME/.env/bin/activate
nohup sudo /opt/mssql/bin/sqlservr start &
sudo /etc/init.d/mysql start
sudo mysql --execute "SET GLOBAL sql_mode=(SELECT REPLACE(@@sql_mode,'ONLY_FULL_GROUP_BY',''));"
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
