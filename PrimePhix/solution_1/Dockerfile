FROM ubuntu:22.04

RUN apt-get update -qq \
    && apt-get install -y unzip wget \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && wget http://phix.x10.mx/phix.1.0.1.zip \
    && wget http://phix.x10.mx/phix.1.0.1.1.zip \
    && wget http://phix.x10.mx/phix.1.0.1.2.zip \
    && wget http://phix.x10.mx/phix.1.0.1.3.zip \
    && wget http://phix.x10.mx/p64 \
    && unzip -o phix.1.0.1.zip -d /usr/local/phix \
    && unzip -o phix.1.0.1.1.zip -d /usr/local/phix \
    && unzip -o phix.1.0.1.2.zip -d /usr/local/phix \
    && unzip -o phix.1.0.1.3.zip -d /usr/local/phix \
    && mv p64 /usr/local/phix/p \
    && chmod +x /usr/local/phix/p \
    && rm -f phix*.zip \
    && printf '#!/bin/bash\n/usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 /usr/local/phix/p "$@"\n' >/usr/local/bin/p \
    && chmod +x /usr/local/bin/p \
    && echo "" | p

WORKDIR /opt/app
COPY primes*.ex run_primes.sh ./

ENTRYPOINT ["./run_primes.sh"]
