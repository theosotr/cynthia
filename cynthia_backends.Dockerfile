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
    ruby-dev

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

WORKDIR ${HOME}
