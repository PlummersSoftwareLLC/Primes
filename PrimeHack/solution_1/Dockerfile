# hhvm docker images only support amd64
FROM hhvm/hhvm:latest

WORKDIR /app

COPY PrimeHack.hack .

ENTRYPOINT [ "hhvm", "PrimeHack.hack" ]