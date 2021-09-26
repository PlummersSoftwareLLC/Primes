FROM library/swipl:8.3.26

WORKDIR /opt/app
COPY primes-*.pl bitvector.c run.sh ./

RUN apt-get update && apt-get --no-install-recommends -y install gcc libc-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && swipl-ld -cc-options,-w,-O3 -shared -o bitvector bitvector.c

ENTRYPOINT [ "./run.sh" ]