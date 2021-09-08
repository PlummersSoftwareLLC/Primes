FROM alpine:3.13

RUN apk add --no-cache build-base=0.5-r2 nasm=2.15.05-r0

WORKDIR /opt/app

COPY *.sh *.asm ./

RUN ./build.sh

ENTRYPOINT [ "./run.sh" ]