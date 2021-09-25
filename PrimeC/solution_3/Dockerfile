FROM ubuntu:18.04

RUN apt-get update && apt-get -y install build-essential

WORKDIR /opt/app

COPY compile.sh .
COPY run.sh .
COPY *.c ./

RUN chmod +x compile.sh run.sh

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]


