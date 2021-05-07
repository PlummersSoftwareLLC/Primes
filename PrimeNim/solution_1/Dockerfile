FROM nimlang/nim:alpine

WORKDIR /opt/app
COPY primes.nim .
RUN nim c -d:release primes.nim

ENTRYPOINT [ "/opt/app/primes" ]
