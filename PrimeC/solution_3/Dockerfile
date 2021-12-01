FROM ubuntu:18.04

RUN apt-get update && apt-get -y install build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY compile.sh run.sh *.c ./

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]