FROM alpine:3.24

WORKDIR /opt/app

RUN apk add --no-cache ghostscript

COPY *.ps *.sh ./

ENTRYPOINT [ "sh", "run.sh" ]
