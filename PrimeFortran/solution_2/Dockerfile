FROM ubuntu:18.04

RUN apt-get update && apt-get -y install make=4.1-9.1ubuntu1 gfortran=4:7.4.0-1ubuntu2.3

WORKDIR /opt/app
COPY Makefile *.f08 ./

RUN make all

ENTRYPOINT ["tail", "-f", "/dev/null"]

