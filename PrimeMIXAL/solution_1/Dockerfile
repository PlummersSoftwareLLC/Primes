FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install --no-install-recommends -y mdk=1.2.9+dfsg-5 time=1.7-25.1build1 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY *.mixal runprime.sh ./

RUN mixasm prime

ENTRYPOINT [ "/opt/app/runprime.sh" ]