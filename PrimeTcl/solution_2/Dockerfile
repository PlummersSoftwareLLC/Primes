FROM alpine:3.13

RUN apk add --update tcl

WORKDIR /opt/app

COPY *.tcl ./
COPY run.sh ./

ENTRYPOINT ["./run.sh" ]
