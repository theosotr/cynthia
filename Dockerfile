FROM ubuntu:18.04

RUN apt update && apt upgrade -yq
RUN apt install -y \
    wget \
    python3-pip \
    sudo \
    sqlite3 \
    libpq-dev \
    libmysqlclient-dev \
    vim

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
ADD ./setup.py  ${HOME}/cynthia_src
ADD ./version.txt  ${HOME}/cynthia_src
ADD ./requirements.txt  ${HOME}/cynthia_src
ADD ./cynthia ${HOME}/cynthia_src/cynthia
WORKDIR ${HOME}/cynthia_src
RUN sudo pip3 install virtualenv
RUN virtualenv -p python3 .env
RUN bash -c "source .env/bin/activate && python3 setup.py install"

WORKDIR ${HOME}
