FROM alpine:3.13

RUN apk add --no-cache build-base

WORKDIR /opt/app

COPY *.sh ./
COPY *.s ./

RUN ./build.sh

ENTRYPOINT [ "./run.sh" ]
