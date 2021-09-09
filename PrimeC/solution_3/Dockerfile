FROM ubuntu:18.04

RUN apt-get update && apt-get -y install build-essential=12.4ubuntu1 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY compile.sh run.sh *.c ./

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]