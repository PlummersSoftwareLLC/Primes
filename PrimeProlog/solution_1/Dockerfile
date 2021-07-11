FROM swipl

WORKDIR /opt/app

COPY primes.pl .

RUN swipl -O -o primes -c ./primes.pl

ENTRYPOINT [ "./primes" ]