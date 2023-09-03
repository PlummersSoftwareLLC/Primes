FROM openjdk:22-slim

RUN apt-get update -qq \
    && apt-get install -y gcc git wget \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY runioscript.txt sievedp.asm *.sh ./

RUN ./build.sh

ENTRYPOINT [ "./run.sh" ]
