FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y gnu-smalltalk

WORKDIR /opt/app

COPY ./primes.st .

ENTRYPOINT ["gst","-g","./primes.st"]
