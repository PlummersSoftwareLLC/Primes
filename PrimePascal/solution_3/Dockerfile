FROM primeimages/freepascal:3.2.0 AS build

COPY *.pas /opt/app/
COPY run.sh /opt/app/
WORKDIR /opt/app

RUN fpc PrimePas -O3 -v0
RUN chmod +x run.sh

FROM ubuntu:20.04
COPY --from=build /opt/app/PrimePas ./
COPY --from=build /opt/app/run.sh ./

ENTRYPOINT [ "./run.sh" ]
