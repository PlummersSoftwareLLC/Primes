FROM ubuntu:22.04

RUN apt-get update -qq \
    && apt-get install -y build-essential bc git \
    && mkdir -p /opt \
    && cd /opt \
    && git clone https://github.com/mist64/cbmbasic.git \
    && cd cbmbasic \
    && make \
    && mv cbmbasic /usr/local/bin \
    && apt-get remove -y build-essential git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY *.bas run_primes.sh ./

ENTRYPOINT [ "./run_primes.sh" ]
