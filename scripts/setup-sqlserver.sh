#! /bin/sh

wget -qO- https://packages.microsoft.com/keys/microsoft.asc | sudo apt-key add - && \
sudo add-apt-repository "$(wget -qO- https://packages.microsoft.com/config/ubuntu/18.04/mssql-server-2019.list)" && \
sudo apt update && \
sudo apt-get install -y mssql-server && \
sudo /opt/mssql/bin/mssql-conf setup && \
systemctl status mssql-server --no-pager && \
curl https://packages.microsoft.com/keys/microsoft.asc | sudo apt-key add - && \
curl https://packages.microsoft.com/config/ubuntu/18.04/prod.list | sudo tee /etc/apt/sources.list.d/msprod.list && \
sudo apt-get update && \
sudo apt-get install mssql-tools unixodbc-dev && \
sudo apt-get -y install libodbc1 unixodbc unixodbc-dev freetds-dev freetds-bin tdsodbc && \
echo 'export PATH="$PATH:/opt/mssql-tools/bin"' >> ~/.bash_profile && \
source ~/.bashrc && \
sqlcmd -S localhost -U orm_testing -P 'orm_testing1' -Q "CREATE LOGIN orm_testing WITH PASSWORD = 'orm_testing1'" && \
sqlcmd -S localhost -U orm_testing -P 'orm_testing1' -Q "EXEC sp_addsrvrolemember 'orm_testing', 'sysadmin'" && \
# sqlcmd -S localhost -U orm_testing -P 'orm_testing1'
