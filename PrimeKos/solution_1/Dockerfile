FROM alpine:3.13 as builder

RUN apk update && \
    apk add curl make g++

ARG KOS_BASENAME=kos-0.0.31-src

WORKDIR /opt

RUN curl https://github.com/kos-lang/kos/releases/download/v0.0.31/"${KOS_BASENAME}".tar.gz -O -L && \
    tar xzf "${KOS_BASENAME}".tar.gz

WORKDIR /opt/"${KOS_BASENAME}"

RUN make install -j"$(nproc)"

WORKDIR /opt

RUN rm -rf "${KOS_BASENAME}"

WORKDIR /opt/app

COPY primes.kos .

ENTRYPOINT [ "./primes.kos" ]
