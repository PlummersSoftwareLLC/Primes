FROM alpine:3.13

RUN apk add --no-cache --update tcl

WORKDIR /opt/app

COPY *.tcl run.sh ./

ENTRYPOINT [ "./run.sh" ]