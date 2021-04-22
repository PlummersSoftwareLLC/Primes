FROM alpine:3.13

RUN apk add --update build-base

WORKDIR /opt/app

COPY compile.sh .
COPY run.sh .
COPY *.c ./

RUN /bin/sh ./compile.sh

ENTRYPOINT [ "/bin/sh","./run.sh" ]


