#! /bin/bash

sudo apt update
sudo apt install mysql-server -y
sudo /etc/init.d/mysql start
sudo mysql --execute  "CREATE USER 'orm_testing'@'localhost' IDENTIFIED BY 'orm_testing'";
sudo mysql --execute "GRANT ALL PRIVILEGES ON *.* TO 'orm_testing'@'localhost'";
