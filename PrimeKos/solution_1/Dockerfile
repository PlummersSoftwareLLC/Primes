FROM alpine:3.13 as builder

RUN apk update && \
    apk add curl make g++

WORKDIR /opt/app

RUN curl https://github.com/kos-lang/kos/releases/download/v0.0.31/kos-0.0.31-src.tar.gz -O -L && \
    tar xzf kos-0.0.31-src.tar.gz

RUN cd kos-0.0.31-src && \
    make install -j$(nproc)

COPY primes.kos .

ENTRYPOINT [ "./primes.kos" ]
