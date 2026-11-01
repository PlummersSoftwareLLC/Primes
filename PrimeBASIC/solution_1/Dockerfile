FROM primeimages/freebasic:1.10.1-ubuntu24.04 AS build

WORKDIR /opt/app
COPY *.bas *.sh ./

RUN find . -name 'prime_*.bas' -exec fbc {} -x {}.run \;

FROM ubuntu:24.04
WORKDIR /opt/app
COPY --from=build /opt/app/*run* ./

ENTRYPOINT [ "./run.sh" ]
