FROM ubuntu:21.04 AS build

RUN apt-get update -qq \
    && apt-get install -y g++

COPY *.cpp /opt/app/
WORKDIR /opt/app
RUN g++ -Ofast -std=c++17 PrimeCPP.cpp -o PrimeCPP

FROM ubuntu:21.04
COPY --from=build /opt/app/PrimeCPP /usr/local/bin

ENTRYPOINT [ "PrimeCPP" ]