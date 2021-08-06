FROM alpine:3.13

RUN apk add --update build-base

WORKDIR /opt/app

COPY compile.sh .
COPY run.sh .
COPY *.c ./

RUN chmod +x compile.sh run.sh

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]


