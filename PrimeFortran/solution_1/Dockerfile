FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y gfortran

WORKDIR /opt/app
COPY PrimesFortran.f03 .

RUN gfortran -v -Ofast -march=native  -o prime PrimesFortran.f03 

ENTRYPOINT [ "./prime" ]
