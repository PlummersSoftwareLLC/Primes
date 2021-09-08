FROM alpine:3.13

RUN apk add --no-cache build-base=0.4-r1 nasm=2.11.08-r0

WORKDIR /opt/app

COPY *.sh ./
COPY *.asm ./

RUN chmod a+x ./*.sh && \
    ./build.sh

ENTRYPOINT [ "./run.sh" ]