FROM ubuntu:20.04 AS build

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -qq \
    && apt-get install -y --no-install-recommends build-essential ca-certificates cmake git libreadline-dev patch python3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY build.sh lci-clock.patch ./
RUN chmod +x build.sh \
    && ./build.sh

COPY lolprime.lol run.sh README.md ./

FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -qq \
    && apt-get install -y --no-install-recommends libreadline8 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY --from=build /opt/app/.build/prefix /opt/lci
COPY lolprime.lol run.sh README.md ./
RUN chmod +x run.sh

ENTRYPOINT ["/opt/app/run.sh"]
