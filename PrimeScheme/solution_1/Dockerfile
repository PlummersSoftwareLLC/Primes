FROM ubuntu:18.04

RUN apt-get update -qq && apt-get install -y chezscheme  \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY . .

ENTRYPOINT [ "./run.ss" ]