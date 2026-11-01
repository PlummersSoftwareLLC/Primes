FROM alpine:3.24

RUN apk --update add --no-cache sbcl

WORKDIR /opt/app

COPY *.lisp .

ENTRYPOINT [ "sbcl", "--script", "PrimeSieve.lisp" ]