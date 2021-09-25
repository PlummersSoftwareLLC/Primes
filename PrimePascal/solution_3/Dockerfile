FROM primeimages/freepascal:3.2.0 AS build

WORKDIR /opt/app
COPY *.pas ./
COPY run.sh ./

RUN chmod a+x run.sh
RUN fpc PrimePas -O3 -v0

FROM ubuntu:20.04
COPY --from=build /opt/app/PrimePas /opt/app/
COPY --from=build /opt/app/run.sh /opt/app/

WORKDIR /opt/app
ENTRYPOINT [ "./run.sh" ]
