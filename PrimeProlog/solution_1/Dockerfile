FROM debian:trixie-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends swi-prolog-core gcc libc6-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY primes-*.pl bitvector.c run.sh ./

RUN swipl-ld -cc-options,-w,-O3 -shared -o bitvector bitvector.c

ENTRYPOINT [ "./run.sh" ]
