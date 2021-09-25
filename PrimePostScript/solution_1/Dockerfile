FROM alpine:3.14

WORKDIR /opt/app

RUN apk add --no-cache ghostscript

COPY *.ps ./
COPY *.sh ./

ENTRYPOINT [ "sh", "run.sh" ]
