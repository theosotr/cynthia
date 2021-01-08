ARG IMAGE_NAME=ubuntu:18.04
FROM ${IMAGE_NAME}

USER cynthia

ENV HOME /home/cynthia
RUN sudo apt install libsqlite3-dev

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
RUN pip3 install virtualenv

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

ADD ./entrypoint/entrypoint.sh /usr/local/bin/
WORKDIR ${HOME}
ENTRYPOINT ["entrypoint.sh"]
