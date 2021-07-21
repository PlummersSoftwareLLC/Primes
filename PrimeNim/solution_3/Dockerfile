FROM nimlang/nim:alpine

WORKDIR /opt/app
COPY primes.nim .
RUN nim c -d:danger --passC:"-march=native" -d:lto --gc:arc primes.nim

ENTRYPOINT [ "/opt/app/primes" ]
