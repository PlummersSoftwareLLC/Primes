FROM ubuntu:21.04 AS build

RUN apt-get update -qq \
    && apt-get install -y clang

COPY *.cpp /opt/app/
WORKDIR /opt/app
RUN clang++ -Wall -Wextra -pedantic -std=c++17 -pthread -O3 -march=native -mtune=native PrimeCPP.cpp -oPrimeCPP

FROM ubuntu:21.04
COPY --from=build /opt/app/PrimeCPP /usr/local/bin

ENTRYPOINT [ "PrimeCPP" ]
