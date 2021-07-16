FROM swipl

RUN apt-get update && apt-get -y install gcc

WORKDIR /opt/app

COPY primes.pl bitvector.c .

RUN swipl-ld -cc-options,-w,-O3 -shared -o bitvector bitvector.c

ENTRYPOINT [ "swipl", "-O", "./primes.pl" ]