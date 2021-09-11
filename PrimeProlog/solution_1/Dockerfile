FROM library/swipl:8.3.26

WORKDIR /opt/app
COPY primes-*.pl bitvector.c run.sh ./

RUN apt-get update && apt-get -y install gcc=4:6.3.0-4 \
    && swipl-ld -cc-options,-w,-O3 -shared -o bitvector bitvector.c

ENTRYPOINT [ "./run.sh" ]
