FROM ubuntu:18.04

RUN apt update && apt upgrade -yq
RUN apt install -y \
    wget \
    python3-pip \
    sudo \
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
