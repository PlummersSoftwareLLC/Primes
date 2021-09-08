FROM alpine:3.13

RUN apk add --no-cache build-base=0.4-r1

WORKDIR /opt/app

COPY *.sh ./
COPY *.s ./

RUN chmod a+x ./*.sh && \
    ./build.sh

ENTRYPOINT [ "./run.sh" ]