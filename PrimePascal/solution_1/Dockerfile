FROM ubuntu:24.04 AS build

RUN apt-get update \
    && apt-get install -y --no-install-recommends fp-compiler libc6-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY *.pas .

RUN fpc prime -O3

FROM ubuntu:24.04
COPY --from=build /opt/app/prime /opt/app/

ENTRYPOINT [ "/opt/app/prime" ]