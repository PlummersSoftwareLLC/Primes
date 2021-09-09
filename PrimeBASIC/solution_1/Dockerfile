FROM primeimages/freebasic:1.07.3 AS build

COPY *.bas *.sh ./
RUN find . -name 'prime_*.bas' -exec fbc {} -x {}.run \;

FROM ubuntu:18.04
WORKDIR /opt/app
COPY --from=build ./*run* ./

ENTRYPOINT [ "./run.sh" ]