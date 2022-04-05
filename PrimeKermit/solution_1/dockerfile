FROM ubuntu:18.04

RUN apt-get update && apt-get -y install ckermit

WORKDIR /opt/app
COPY *.ksc ./

ENTRYPOINT [ "kermit", "+", "/opt/app/PrimeSieve.ksc", "1000000" ]