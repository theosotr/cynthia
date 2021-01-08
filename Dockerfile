ARG IMAGE_NAME=ubuntu:18.04
FROM ${IMAGE_NAME}

ADD ./scripts/setup-orms.sh ${HOME}/scripts/setup-orms.sh

# Add source code
RUN mkdir ${HOME}/cynthia_src
ADD ./build.sbt  ${HOME}/cynthia_src
ADD ./src  ${HOME}/cynthia_src/src
ADD ./project  ${HOME}/cynthia_src/project
ADD ./Makefile ${HOME}/cynthia_src/Makefile
ADD ./lib ${HOME}/cynthia_src/lib

WORKDIR ${HOME}/cynthia_src

# Setup a Python3 virtual environment.
RUN sudo pip3 install virtualenv
RUN ${HOME}/scripts/setup-orms.sh ${HOME}

RUN sudo chown -R cynthia:cynthia ${HOME}/cynthia_src

# Build project
RUN make

WORKDIR ${HOME}
