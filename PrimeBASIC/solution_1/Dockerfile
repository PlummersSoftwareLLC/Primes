FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y gcc libncurses5-dev libffi-dev libgl1-mesa-dev libx11-dev libxext-dev libxrender-dev libxrandr-dev libxpm-dev

ADD FreeBASIC-1.07.3-linux-x86_64.tar.xz /fbc-install

WORKDIR /fbc-install/FreeBASIC-1.07.3-linux-x86_64
RUN ./install.sh -i

WORKDIR /
RUN rm -rf /fbc-install

WORKDIR /opt/app
COPY *.bas .
RUN fbc prime.bas

ENTRYPOINT [ "/opt/app/prime" ]