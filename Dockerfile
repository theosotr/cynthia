FROM ubuntu:18.04

ENV TZ=Europe/Athens
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt update && apt upgrade -yq
RUN apt install -y \
    wget \
    python3-pip \
    sudo \
    sqlite3 \
    libpq-dev \
    libmysqlclient-dev \
    default-jdk \
    scala \
    curl \
    vim \
    software-properties-common \
    build-essential \
    systemd \
    ruby-full \
    ruby-dev \
    libsqlite3-dev

# Install node
RUN curl -sL https://deb.nodesource.com/setup_14.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
RUN apt install -y nodejs

# Install sbt
RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt update && apt install sbt -y

# Create the cynthia user.
RUN useradd -ms /bin/bash cynthia && \
    echo cynthia:cynthia | chpasswd && \
    cp /etc/sudoers /etc/sudoers.bak && \
    echo 'cynthia ALL=(ALL:ALL) NOPASSWD:ALL' >> /etc/sudoers
USER cynthia
ENV HOME /home/cynthia
WORKDIR ${HOME}

RUN mkdir ${HOME}/scripts

# Setup backends
ADD ./scripts/setup-sqlserver.sh ${HOME}/scripts/setup-sqlserver.sh
RUN ${HOME}/scripts/setup-sqlserver.sh

ADD ./scripts/setup-postgres.sh ${HOME}/scripts/setup-postgres.sh
RUN ${HOME}/scripts/setup-postgres.sh

ADD ./scripts/setup-mysql.sh ${HOME}/scripts/setup-mysql.sh
RUN ${HOME}/scripts/setup-mysql.sh

ADD ./scripts/setup-z3.sh ${HOME}/scripts/setup-z3.sh
RUN ${HOME}/scripts/setup-z3.sh

USER cynthia

ADD ./patches/ ${HOME}/patches

# Add source code
RUN mkdir ${HOME}/cynthia_src
RUN mkdir ${HOME}/cynthia_src/scripts
ADD ./build.sbt  ${HOME}/cynthia_src
ADD ./src  ${HOME}/cynthia_src/src
ADD ./project  ${HOME}/cynthia_src/project
ADD ./Makefile ${HOME}/cynthia_src/Makefile
ADD ./scripts/cynthia ${HOME}/cynthia_src/scripts/
ADD ./lib ${HOME}/cynthia_src/lib

RUN sudo chown -R cynthia:cynthia ${HOME}
WORKDIR ${HOME}/cynthia_src

# Build project
RUN make
RUN sudo make install

ENV CYNTHIA_JAR_FILE ${HOME}/cynthia_src/scripts/cynthia-assembly-0.1.0-SNAPSHOT.jar

WORKDIR ${HOME}
ADD ./scripts/setup-orms.sh ${HOME}/scripts/setup-orms.sh

# Setup a Python3 virtual environment.
RUN sudo pip3 install virtualenv

# Clone Python ORMs
RUN git clone https://github.com/django/django
RUN git clone https://github.com/sqlalchemy/sqlalchemy
RUN git clone https://github.com/coleifer/peewee && \
  cd peewee && \
  patch < ${HOME}/patches/pwiz.patch && \
  patch < ${HOME}/patches/peewee.patch

# Install necessary npm packages
RUN sudo npm install --unsafe-perm -g \
  sqlite3 \
  pg pg-hstore \
  mysql \
  tedious \
  sequelize \
  sequelize-auto

RUN npm install --save \
  sqlite3 \
  pg pg-hstore \
  mysql \
  tedious \
  sequelize

# Install ruby packages
RUN sudo gem install sqlite3 mysql2 postgres activerecord pg \
    activerecord-cockroachdb-adapter ruby-odbc sql_server
RUN git clone https://github.com/theosotr/rmre && \
  cd rmre && \
  gem build rmre.gemspec && \
  sudo gem install rmre-0.0.9.gem

RUN sudo chown cynthia:cynthia -R $HOME/.config
RUN  bash -c "virtualenv -p python3 $HOME/.env && \
  source $HOME/.env/bin/activate && \
  pip install mysqlclient psycopg2 sqlacodegen pyodbc django-mssql-backend && \
  $HOME/scripts/setup-orms.sh ${HOME}"

ADD ./examples ${HOME}/cynthia_src
ADD ./entrypoint/entrypoint.sh /usr/local/bin/

# Set up locales
RUN sudo apt install -y locales
# Set the locale
RUN sudo sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    sudo locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8     

USER cynthia
WORKDIR ${HOME}
ENTRYPOINT ["entrypoint.sh"]
