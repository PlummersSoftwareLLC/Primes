FROM python:3.9

WORKDIR /root

RUN [ "pip", "install", "cython"]

COPY Prime32.pyx build.sh ./

RUN [ "./build.sh" ]

ENTRYPOINT ./sieve
