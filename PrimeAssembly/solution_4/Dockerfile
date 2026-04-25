FROM alpine:3.13

RUN apk add --no-cache build-base nasm

WORKDIR /opt/app

COPY *.sh *.asm README.md ./

RUN ./build.sh

ENTRYPOINT ["./run.sh"]
