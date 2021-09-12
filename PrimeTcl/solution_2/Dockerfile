FROM alpine:3.13

RUN apk add --no-cache --update tcl=8.6.10-r1

WORKDIR /opt/app

COPY *.tcl run.sh ./

ENTRYPOINT [ "./run.sh" ]