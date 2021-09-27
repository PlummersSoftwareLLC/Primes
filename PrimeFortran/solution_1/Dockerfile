FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y gfortran \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY PrimesFortran.f08 .

RUN gfortran -v -Ofast -march=native  -o prime PrimesFortran.f08 

ENTRYPOINT [ "./prime" ]