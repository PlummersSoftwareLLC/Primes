FROM ubuntu:20.04 AS build

ENV HARBOUR_PKG=r2017-12-15-18_53_harbour_3.2.0-1_amd64.deb

WORKDIR /opt

RUN apt-get update && \
    apt-get install -y wget build-essential && \
    wget "https://sourceforge.net/projects/harbour-project/files/binaries-linux-ubuntu/nightly/${HARBOUR_PKG}/download" -O ${HARBOUR_PKG} && \
    apt-get install -y "./${HARBOUR_PKG}" && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    rm "${HARBOUR_PKG}" 

COPY . .

RUN hbmk2 -gtstd -optim -cflag=-O3 ./sieve.prg \
    && hbmk2 -gtstd -optim -cflag=-O3 ./sievedb.prg \
    && hbmk2 -gtstd -optim -cflag=-O3 ./sieve_xharbour.prg

CMD [ "bash", "./run.sh" ]
