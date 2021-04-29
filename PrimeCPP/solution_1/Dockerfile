FROM ubuntu:18.04 AS build

RUN apt-get update -qq \
    && apt-get install -y clang

COPY *.cpp /opt/app/
WORKDIR /opt/app
RUN clang++ -Ofast -std=c++17 PrimeCPP.cpp -oPrimeCPP

FROM ubuntu:18.04
COPY --from=build /opt/app/PrimeCPP /usr/local/bin

ENTRYPOINT [ "PrimeCPP" ]