FROM alpine:3.13 as builder

RUN apk update && \
    apk add --no-cache curl make g++

ARG KOS_BASENAME=kos-0.0.31-src

WORKDIR /opt/app

RUN curl https://github.com/kos-lang/kos/releases/download/v0.0.31/"${KOS_BASENAME}".tar.gz -O -L \
    && tar xzf "${KOS_BASENAME}".tar.gz \
    && make install -C "${KOS_BASENAME}" -j"$(nproc)" \
    && rm -rf "${KOS_BASENAME}"*

COPY primes.kos .

ENTRYPOINT [ "./primes.kos" ]
