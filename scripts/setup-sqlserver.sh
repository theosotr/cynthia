#! /bin/bash

MSSQL_SA_PASSWORD='orm_testing1'

wget -qO- https://packages.microsoft.com/keys/microsoft.asc |
sudo apt-key add -
sudo add-apt-repository "$(wget -qO- https://packages.microsoft.com/config/ubuntu/18.04/mssql-server-2019.list)"
sudo apt update
sudo apt-get install -y mssql-server
sudo MSSQL_SA_PASSWORD=$MSSQL_SA_PASSWORD /opt/mssql/bin/mssql-conf -n setup accept-eula
systemctl status mssql-server --no-pager

curl https://packages.microsoft.com/keys/microsoft.asc |
sudo apt-key add -

curl https://packages.microsoft.com/config/ubuntu/18.04/prod.list |
sudo tee /etc/apt/sources.list.d/msprod.list

sudo apt-get update
sudo ACCEPT_EULA=Y apt-get install -y mssql-tools unixodbc-dev && \
sudo apt-get -y install libodbc1 unixodbc unixodbc-dev freetds-dev freetds-bin tdsodbc

export PATH="$PATH:/opt/mssql-tools/bin/"
echo 'export PATH="$PATH:/opt/mssql-tools/bin"' >> ~/.zshrc

sqlcmd -S localhost -U SA -P "$MSSQL_SA_PASSWORD" -Q "CREATE LOGIN orm_testing WITH PASSWORD = '$MSSQL_SA_PASSWORD'"
sqlcmd -S localhost -U SA -P "$MSSQL_SA_PASSWORD" -Q "EXEC sp_addsrvrolemember 'orm_testing', 'sysadmin'"
