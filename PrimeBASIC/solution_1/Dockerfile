FROM primeimages/freebasic:1.07.3 AS build

WORKDIR /opt/app
COPY *.bas *.sh ./

RUN find . -name 'prime_*.bas' -exec fbc {} -x {}.run \;

FROM ubuntu:18.04
WORKDIR /opt/app
COPY --from=build /opt/app/*run* ./

ENTRYPOINT [ "./run.sh" ]