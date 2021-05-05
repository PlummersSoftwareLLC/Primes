FROM alpine:3.13

RUN apk --update add --no-cache sbcl

WORKDIR /opt/app

COPY *.lisp .

ENTRYPOINT [ "sbcl", "--script", "PrimeSieve.lisp" ]