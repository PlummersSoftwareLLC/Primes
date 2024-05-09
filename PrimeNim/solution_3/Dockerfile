FROM primeimages/nim:1.4.8

WORKDIR /opt/app
COPY Primes.nim .
RUN nim c --gc:arc -d:danger -t:-march=native -d:lto --threads:on Primes.nim

ENTRYPOINT [ "/opt/app/Primes" ]
