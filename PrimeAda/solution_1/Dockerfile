FROM ubuntu:18.04 AS build

RUN apt-get update -qq \
    && apt-get install -y gprbuild gnat

COPY . /opt/app/
WORKDIR /opt/app
RUN mkdir obj
RUN gprbuild -P primesieveada.gpr

FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y libgnat-7

COPY --from=build /opt/app/obj/main /usr/local/bin/prime
ENTRYPOINT [ "prime" ]