FROM ubuntu:22.04 AS build

RUN apt-get update -qq \
    && apt-get install -y clang
COPY *.s *.sh /opt/app/
WORKDIR /opt/app
RUN ./build.sh

FROM ubuntu:22.04
COPY --from=build /opt/app/primes /usr/local/bin

ENTRYPOINT [ "primes" ]