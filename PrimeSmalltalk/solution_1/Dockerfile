FROM ubuntu:18.04

# small talk is not on Alpine repositories
RUN apt-get update -qq \
    && apt-get install -y gnu-smalltalk

WORKDIR /opt/app

COPY ./primes.st .

ENTRYPOINT ["gst","./primes.st"]
#ENTRYPOINT ["/bin/bash"]
