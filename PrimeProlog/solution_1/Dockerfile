FROM library/swipl:8.3.26

RUN apt-get update && apt-get -y install gcc

WORKDIR /opt/app

COPY primes-*.pl bitvector.c run.sh .

RUN swipl-ld -cc-options,-w,-O3 -shared -o bitvector bitvector.c

ENTRYPOINT [ "./run.sh" ]
