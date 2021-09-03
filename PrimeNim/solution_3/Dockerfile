FROM primeimages/nim:1.4.8

WORKDIR /opt/app
COPY primes.nim .
RUN nim c --gc:arc -d:danger -t:-march=native -d:lto primes.nim

ENTRYPOINT [ "/opt/app/primes" ]
