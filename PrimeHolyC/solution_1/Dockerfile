# Holy C Sieve - TempleOS Runtime
# Note: Holy C is specific to TempleOS and cannot run in standard Docker containers.

FROM ubuntu:24.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && \
    apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

RUN git clone https://github.com/Jamesbarford/holyc-lang.git && \
    cd holyc-lang && \
    make && \
    make install

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && \
    apt-get update && apt-get install -y \
    gcc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/primes

COPY --from=builder /usr/local/bin/hcc /usr/local/bin/hcc
COPY --from=builder /usr/local/include /usr/local/include
COPY --from=builder /usr/local/lib /usr/local/lib
COPY --from=builder /usr/local/share /usr/local/share

COPY sieve.HC ./

RUN hcc sieve.HC -o sieve

COPY run.sh ./
RUN sed -i 's/\r$//' run.sh && chmod +x run.sh

ENTRYPOINT ["./run.sh"]
