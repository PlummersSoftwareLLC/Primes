FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y ruby

WORKDIR /opt/app
COPY *.rb .

ENTRYPOINT [ "ruby", "/opt/app/prime.rb" ]