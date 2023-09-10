FROM ubuntu:22.04

RUN apt-get update -qq \
    && apt-get install -y build-essential bc git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY *.bas *.sh ./

RUN ./build.sh

ENTRYPOINT [ "./run.sh" ]
