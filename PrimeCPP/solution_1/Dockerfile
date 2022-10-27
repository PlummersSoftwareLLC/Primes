FROM ubuntu:22.04 AS build

RUN apt-get update -qq \
    && apt-get install -y clang

WORKDIR /opt/app
COPY *.cpp .
RUN clang++ -march=native -mtune=native -Ofast -std=c++17 PrimeCPP.cpp -o PrimeCPP

FROM ubuntu:22.04
COPY --from=build /opt/app/PrimeCPP /usr/local/bin

ENTRYPOINT [ "PrimeCPP" ]