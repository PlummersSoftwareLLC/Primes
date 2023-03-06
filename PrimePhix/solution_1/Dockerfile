FROM ubuntu:22.04

RUN apt-get update -qq \
    && apt-get install -y unzip wget \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && wget http://phix.x10.mx/phix.1.0.2.zip \
    && wget http://phix.x10.mx/phix.1.0.2.1.zip \
    && wget http://phix.x10.mx/phix.1.0.2.2.zip \
    && wget http://phix.x10.mx/phix.1.0.2.3.zip \
    && wget http://phix.x10.mx/phix.1.0.2.4.zip \
    && wget http://phix.x10.mx/p64 \
    && unzip phix.1.0.2.zip -d /usr/local/phix \
    && unzip phix.1.0.2.1.zip -d /usr/local/phix \
    && unzip phix.1.0.2.2.zip -d /usr/local/phix \
    && unzip phix.1.0.2.3.zip -d /usr/local/phix \
    && unzip phix.1.0.2.4.zip -d /usr/local/phix \
    && mv p64 /usr/local/phix/p \
    && chmod +x /usr/local/phix/p \
    && rm -f phix*.zip \
    && printf '#!/bin/bash\n/usr/local/phix/p "$@"\n' >/usr/local/bin/p \
    && chmod +x /usr/local/bin/p \
    && echo "" | p

WORKDIR /opt/app
COPY primes*.ex run_primes.sh ./
RUN p -c -norun primes.ex && \
    p -c -norun primes_bit.ex

ENTRYPOINT ["./run_primes.sh"]
