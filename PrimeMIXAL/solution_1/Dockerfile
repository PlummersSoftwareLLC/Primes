FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y mdk time \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY *.mixal runprime.sh ./

RUN mixasm prime

ENTRYPOINT [ "/opt/app/runprime.sh" ]