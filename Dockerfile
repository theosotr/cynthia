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
    systemd

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
ADD ./scripts ${HOME}/scripts

# Setup backends
#RUN ${HOME}/scripts/setup-sqlserver.sh
RUN ${HOME}/scripts/setup-postgres.sh
RUN ${HOME}/scripts/setup-mysql.sh
RUN ${HOME}/scripts/setup-z3.sh

RUN mkdir ${HOME}/cynthia_src

# Add source code
ADD ./requirements.txt ${HOME}/cynthia_src
ADD ./build.sbt  ${HOME}/cynthia_src
ADD ./src  ${HOME}/cynthia_src/src
ADD ./project  ${HOME}/cynthia_src/project
ADD ./Makefile ${HOME}/cynthia_src/Makefile
ADD ./lib ${HOME}/cynthia_src/lib


WORKDIR ${HOME}/cynthia_src

# Setup a Python3 virtual environment.
RUN sudo pip3 install virtualenv
RUN virtualenv -p python3 .env
RUN bash -c "source .env/bin/activate && pip install -r requirements.txt"

RUN sudo chown -R cynthia:cynthia ${HOME}/cynthia_src

# Build project
RUN make

WORKDIR ${HOME}
