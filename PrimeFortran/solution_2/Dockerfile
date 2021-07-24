FROM ubuntu:18.04

RUn apt-get update && apt-get -y install make gfortran

WORKDIR /opt/app
COPY Makefile *.f08 ./

RUN make all

ENTRYPOINT [ "make", "run" ]
