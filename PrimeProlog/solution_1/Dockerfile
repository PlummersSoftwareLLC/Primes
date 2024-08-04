FROM library/swipl:9.2.6

WORKDIR /opt/app
COPY primes-*.pl bitvector.c run.sh ./

RUN apt-get update && apt-get -y install gcc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && swipl-ld -cc-options,-w,-O3 -shared -o bitvector bitvector.c

ENTRYPOINT [ "./run.sh" ]
