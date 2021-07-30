FROM primeimages/freepascal:3.2.0 AS build

WORKDIR /opt/app
COPY *.pas .

RUN fpc PrimePas4.pas -O2 -v0

FROM ubuntu:20.04
COPY --from=build /opt/app/PrimePas4 /opt/app/

ENTRYPOINT [ "/opt/app/PrimePas4" ]
 
