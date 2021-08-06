# hhvm docker images only support amd64
FROM hhvm/hhvm:4.121.0

WORKDIR /app

COPY PrimeHack.hack .

ENTRYPOINT [ "hhvm", "PrimeHack.hack" ]