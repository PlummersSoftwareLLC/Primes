FROM ubuntu:18.04

RUN apt-get update && apt-get -y install make gfortran \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY Makefile *.f08 ./

RUN make all

ENTRYPOINT [ "make", "run" ]