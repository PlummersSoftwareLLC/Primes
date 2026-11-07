FROM ubuntu:24.04

ARG HARBOUR_VERSION=v3.2.0

RUN apt-get update \
    && apt-get install -y build-essential ca-certificates git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Build Harbour from its release tag; the prebuilt Ubuntu packages need
# libncurses5, which Ubuntu 24.04 no longer ships.
RUN git clone --depth 1 --branch "${HARBOUR_VERSION}" https://github.com/harbour/core /tmp/harbour \
    && make -C /tmp/harbour -j"$(nproc)" install HB_INSTALL_PREFIX=/usr/local \
    && rm -rf /tmp/harbour \
    && echo /usr/local/lib/harbour > /etc/ld.so.conf.d/harbour.conf \
    && ldconfig

WORKDIR /opt

COPY . .

RUN hbmk2 -gtstd -optim -cflag=-O3 ./sieve.prg \
    && hbmk2 -gtstd -optim -cflag=-O3 ./sievedb.prg \
    && hbmk2 -gtstd -optim -cflag=-O3 ./sieve_xharbour.prg

CMD [ "bash", "./run.sh" ]
