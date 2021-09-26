FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install --no-install-recommends -y gnu-smalltalk \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY ./primes.st .

ENTRYPOINT ["gst","-g","./primes.st"]
