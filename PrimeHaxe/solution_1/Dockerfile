FROM haxe:4.2.1-alpine3.13

RUN apk update && apk add --no-cache g++ python3 musl-locales bash
RUN ln -s /usr/include/locale.h /usr/include/xlocale.h

WORKDIR /opt/app
COPY . .

RUN haxelib install hxcpp

COPY compile_and_run_all.sh .
RUN chmod a+x compile_and_run_all.sh

ENTRYPOINT [ "/opt/app/compile_and_run_all.sh" ]
