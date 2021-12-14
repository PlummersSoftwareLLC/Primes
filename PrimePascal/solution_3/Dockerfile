FROM primeimages/freepascal:3.2.0 AS build

WORKDIR /opt/app
COPY *.pas run.sh ./

RUN fpc PrimePas -O3 -v0

FROM ubuntu:20.04
WORKDIR /opt/app
COPY --from=build /opt/app/PrimePas /opt/app/run.sh /opt/app/

ENTRYPOINT [ "./run.sh" ]
