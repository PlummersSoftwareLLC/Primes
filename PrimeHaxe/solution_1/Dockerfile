FROM haxe:4.2.1-alpine3.13

RUN apk update && apk add --no-cache g++ python3 musl-locales bash && \
    ln -s /usr/include/locale.h /usr/include/xlocale.h

WORKDIR /opt/app
COPY . .

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]