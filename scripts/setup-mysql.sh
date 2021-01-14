#! /bin/bash

sudo apt update -yqq
sudo apt install -yqq wget lsb-release gnupg expect
wget -c https://dev.mysql.com/get/mysql-apt-config_0.8.11-1_all.deb
/home/cynthia/scripts/mysql.exp
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 8C718D3B5072E1F5
sudo apt update
export DEBIAN_FRONTEND=noninteractive
sudo -E apt-get -q -y install mysql-server
rm mysql-apt-config_0.8.11-1_all.deb
sudo rm -rf /var/lib/mysql
sudo mysqld --initialize-insecure --user=mysql
sudo /usr/bin/mysqld_safe >/dev/null 2>&1 &
# wait for mysql server to start (max 30 seconds)
timeout=30
echo -n "Waiting for database server to accept connections"
while ! /usr/bin/mysqladmin -u root status >/dev/null 2>&1
do
  timeout=$(($timeout - 1))
  if [ $timeout -eq 0 ]; then
    echo -e "\nCould not connect to database server. Aborting..."
    exit 1
  fi
  echo -n "."
  sleep 1
done
echo
sudo mysql --execute "CREATE USER 'orm_testing'@'localhost' IDENTIFIED BY 'orm_testing'";
sudo mysql --execute "GRANT ALL PRIVILEGES ON *.* TO 'orm_testing'@'localhost'";
