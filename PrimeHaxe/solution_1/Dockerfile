FROM haxe:4.2.1-alpine3.13

RUN apk update && apk add --no-cache g++=10.2.1_pre1-r3 python3=3.8.10-r0 musl-locales=0_git20200319-r1 bash=5.1.0-r0 && \
    ln -s /usr/include/locale.h /usr/include/xlocale.h

WORKDIR /opt/app
COPY . .

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]