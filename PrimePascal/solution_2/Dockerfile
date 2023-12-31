FROM primeimages/freepascal:3.2.2 AS build

WORKDIR /opt/app
COPY *.pas .

RUN fpc prime

FROM ubuntu:22.04
COPY --from=build /opt/app/prime /opt/app/

ENTRYPOINT [ "/opt/app/prime" ]