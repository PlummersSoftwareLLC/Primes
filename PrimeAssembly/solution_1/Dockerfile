FROM alpine:3.13

RUN apk add --no-cache build-base nasm

WORKDIR /opt/app

COPY *.sh ./
COPY *.asm ./

RUN chmod a+x ./*.sh && \
    ./build.sh

ENTRYPOINT [ "./run.sh" ]


