#! /bin/bash

set +e

# Initialize Backends
echo "Initialize Backends"

source $HOME/.env/bin/activate
echo "  * Starting MSSQL database server"
nohup sudo /opt/mssql/bin/sqlservr start &
sudo sh -c "echo "[mysqld]" >> /etc/mysql/my.cnf"
sudo sh -c "echo "skip-grant-tables" >> /etc/mysql/my.cnf"
sudo /etc/init.d/mysql start
sudo mysql --execute "SET GLOBAL sql_mode=(SELECT REPLACE(@@sql_mode,'ONLY_FULL_GROUP_BY',''));"

timeout=30
while ! sudo /etc/init.d/postgresql start 2>&1
do
  timeout=$(($timeout - 1))
  if [ $timeout -eq 0 ]; then
    echo -e "\nCould not connect to PostgreSQL. Aborting..."
    exit 1
  fi
done

source $HOME/.env/bin/activate
if [ $# -eq 0  ]; then
  /bin/bash
  exit 0
fi
echo
echo
echo "Running $@"
eval "$@"
