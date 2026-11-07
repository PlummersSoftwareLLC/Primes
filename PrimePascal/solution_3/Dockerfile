FROM ubuntu:24.04 AS build

RUN apt-get update \
    && apt-get install -y --no-install-recommends fp-compiler libc6-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY *.pas run.sh ./

RUN fpc PrimePas -O3 -v0

FROM ubuntu:24.04
WORKDIR /opt/app
COPY --from=build /opt/app/PrimePas /opt/app/run.sh /opt/app/

ENTRYPOINT [ "./run.sh" ]
