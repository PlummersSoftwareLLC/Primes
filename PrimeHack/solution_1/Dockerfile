# primeimages/hhvm is only built for amd64
FROM primeimages/hhvm:4.172.3-ubuntu24.04

WORKDIR /app

COPY PrimeHack.hack .

ENTRYPOINT [ "hhvm", "-vEval.JitPGO=0", "PrimeHack.hack" ]
