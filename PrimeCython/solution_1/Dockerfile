FROM python:3.9

WORKDIR /root

RUN [ "pip", "install", "cython"]

COPY 8bit/PrimeCY.pyx .
COPY 1bit/Prime32.pyx .
COPY build.sh .
COPY run.sh .

RUN ["./build.sh"]

ENTRYPOINT [ "./run.sh" ]
