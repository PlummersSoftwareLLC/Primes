FROM alpine:3.13

RUN apk add --no-cache build-base=0.5-r2

WORKDIR /opt/app

COPY *.s* ./

RUN ./build.sh

ENTRYPOINT [ "./run.sh" ]