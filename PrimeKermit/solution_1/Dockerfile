# ubuntu 18.04 is selected as newer LTS do not have ckermit in repos
FROM ubuntu:18.04

RUN apt-get update && apt-get -y install ckermit\
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY *.ksc ./

ENTRYPOINT [ "kermit", "+", "/opt/app/PrimeSieve.ksc", "1000000" ]