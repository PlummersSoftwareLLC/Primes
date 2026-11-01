# not ideal to use wine, but no other choice on unix
FROM debian:trixie-slim

ARG nmax=1000000
ARG vhd=n
ENV NMAX=${nmax} VHD=${vhd} WINEDEBUG=-all

RUN apt-get update \
    && apt-get install -y --no-install-recommends wine wine64 \
    && rm -rf /var/lib/apt/lists/*

# Create the Wine prefix at build time, so it isn't done (and timed) at run time
RUN wineboot --init \
    && wineserver -w

WORKDIR /opt/app
COPY *.cmd ./

ENTRYPOINT [ "sh", "-c", "wine cmd /c PrimeFiles.cmd NMAX=$NMAX VHD=$VHD" ]
