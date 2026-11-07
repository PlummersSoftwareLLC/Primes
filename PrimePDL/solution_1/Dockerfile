FROM debian:trixie-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends pdl libinline-c-perl gcc make libc6-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY *.pl .

ENTRYPOINT [ "perl", "primes.pl" ]
