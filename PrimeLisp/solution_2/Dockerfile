FROM alpine:3.24

RUN apk add --no-cache sbcl

WORKDIR /opt/app

COPY *.lisp run.sh ./

ENTRYPOINT [ "./run.sh" ]
