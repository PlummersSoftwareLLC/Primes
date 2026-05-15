FROM ubuntu:24.04

RUN apt-get update -qq \
    && apt-get install -y --no-install-recommends bash build-essential intercal \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY cwager_intercal.i helper.c build.sh run.sh ./
RUN chmod +x build.sh run.sh \
    && ./build.sh

ENTRYPOINT [ "./run.sh" ]
