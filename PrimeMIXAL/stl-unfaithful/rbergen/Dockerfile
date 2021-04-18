FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y mdk dos2unix

WORKDIR /opt/app
COPY *.mixal .

RUN find . -name *.mixal -exec dos2unix {} \; 
RUN mixasm primes

ENTRYPOINT [ "mixvm", "-r", "/opt/app/primes" ]
