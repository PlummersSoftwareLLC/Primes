FROM alpine:3.13

RUN apk add --update build-base

WORKDIR /opt/app

COPY *.sh ./
COPY *.s ./

RUN chmod a+x *.sh

RUN ./build.sh

ENTRYPOINT [ "./run.sh" ]


