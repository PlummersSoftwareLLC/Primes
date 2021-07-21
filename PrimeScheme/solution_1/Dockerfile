FROM ubuntu:18.04

RUN apt-get update -qq && apt-get install -y chezscheme

WORKDIR /opt/app
COPY * ./

ENTRYPOINT [ "./run.ss" ]
