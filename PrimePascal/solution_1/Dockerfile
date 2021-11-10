FROM primeimages/freepascal:3.2.0 AS build

WORKDIR /opt/app
COPY *.pas .

RUN fpc prime -O3

FROM ubuntu:20.04
COPY --from=build /opt/app/prime /opt/app/

ENTRYPOINT [ "/opt/app/prime" ]