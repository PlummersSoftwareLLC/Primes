FROM alpine:3.14

WORKDIR /opt/app

RUN apk add --no-cache ghostscript=9.54.0-r0

COPY *.ps *.sh ./

ENTRYPOINT [ "sh", "run.sh" ]
