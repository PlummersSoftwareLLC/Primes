FROM alpine:3.13

RUN apk --update add --no-cache sbcl=2.0.11-r0

WORKDIR /opt/app

COPY *.lisp .

ENTRYPOINT [ "sbcl", "--script", "PrimeSieve.lisp" ]