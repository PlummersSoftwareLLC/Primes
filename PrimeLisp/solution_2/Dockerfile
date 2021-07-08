FROM alpine:3.13

RUN apk --update add --no-cache sbcl

WORKDIR /opt/app

COPY *.lisp .
COPY run.sh .

RUN chmod +x run.sh

ENTRYPOINT [ "./run.sh" ]
