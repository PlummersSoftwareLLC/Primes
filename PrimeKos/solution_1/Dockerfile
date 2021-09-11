FROM alpine:3.13 as builder

RUN apk update && \
    apk add --no-cache curl=7.78.0-r0 make=4.3-r0 g++=10.2.1_pre1-r3 

ARG KOS_BASENAME=kos-0.0.31-src

WORKDIR /opt/app

RUN curl https://github.com/kos-lang/kos/releases/download/v0.0.31/"${KOS_BASENAME}".tar.gz -O -L \
    && tar xzf "${KOS_BASENAME}".tar.gz \
    && make install -C "${KOS_BASENAME}" -j"$(nproc)" \
    && rm -rf "${KOS_BASENAME}"*

COPY primes.kos .

ENTRYPOINT [ "./primes.kos" ]