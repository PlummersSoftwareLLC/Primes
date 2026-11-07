# not ideal to use wine, but no other choice on unix
FROM debian:trixie-slim

ARG workers=1
ARG sieveSize=1000000
ENV WORKERS=${workers} SIEVE_SIZE=${sieveSize} WINEDEBUG=-all

RUN apt-get update \
    && apt-get install -y --no-install-recommends wine wine64 \
    && rm -rf /var/lib/apt/lists/*

# Create the Wine prefix at build time, so it isn't done (and timed) at run time
RUN wineboot --init \
    && wineserver -w

WORKDIR /opt/app
COPY *.bat ./

ENTRYPOINT [ "sh", "-c", "wine cmd /c main.bat /workers:$WORKERS /sieveSize:$SIEVE_SIZE" ]
