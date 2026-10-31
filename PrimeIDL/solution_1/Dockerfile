FROM ubuntu:24.04

RUN apt-get update \
    && apt-get install -y --no-install-recommends gnudatalanguage \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY *.pro run.sh ./

ENTRYPOINT [ "./run.sh" ]
