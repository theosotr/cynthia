FROM ubuntu:18.04

RUN apt update && apt upgrade -yq
RUN apt install -y \
    wget \
    python3-pip \
    sudo \
    sqlite3 \
    libpq-dev \
    libmysqlclient-dev \
    scala \
    curl \
    vim

# Install sbt
RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt update && apt install sbt -y

# Create the orm user.
RUN useradd -ms /bin/bash orm && \
    echo orm:orm | chpasswd && \
    cp /etc/sudoers /etc/sudoers.bak && \
    echo 'orm ALL=(ALL:ALL) NOPASSWD:ALL' >> /etc/sudoers
USER orm
ENV HOME /home/orm
WORKDIR ${HOME}

RUN mkdir ${HOME}/scripts
ADD ./scripts ${HOME}/scripts
RUN ${HOME}/scripts/setup-postgres.sh
RUN ${HOME}/scripts/setup-mysql.sh

RUN mkdir ${HOME}/cynthia_src

ADD ./requirements.txt ${HOME}/cynthia_src
ADD ./build.sbt  ${HOME}/cynthia_src
ADD ./src  ${HOME}/cynthia_src/src
ADD ./project  ${HOME}/cynthia_src/project
ADD ./examples ${HOME}/cynthia_src/examples
WORKDIR ${HOME}/cynthia_src
# Setup a Python3 virtual environment.
RUN sudo pip3 install virtualenv
RUN virtualenv -p python3 .env
RUN bash -c "source .env/bin/activate && pip install -r requirements.txt"

RUN sudo chown -R orm:orm ${HOME}/cynthia_src
RUN sbt assembly

WORKDIR ${HOME}
