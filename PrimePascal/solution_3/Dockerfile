FROM primeimages/freepascal:3.2.0 AS build

COPY *.pas /opt/app/
WORKDIR /opt/app

RUN fpc PrimePas -O3 -v0

FROM ubuntu:20.04
COPY --from=build /opt/app/PrimePas /opt/app/

ENTRYPOINT [ "/opt/app/PrimePas" ]
