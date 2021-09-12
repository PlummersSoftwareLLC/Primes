FROM ubuntu:18.04

RUN apt-get update && apt-get --no-install-recommends -y install make=4.1-9.1ubuntu1 gfortran=4:7.4.0-1ubuntu2.3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY Makefile *.f08 ./

RUN make all

ENTRYPOINT [ "make", "run" ]