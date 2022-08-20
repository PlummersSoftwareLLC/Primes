FROM ubuntu:22.04 AS build

RUN apt-get update -qq \
    && apt-get install -y clang
COPY *.cpp *.h *.sh /opt/app/
WORKDIR /opt/app
RUN ./build.sh

FROM ubuntu:22.04
COPY --from=build /opt/app/primes_constexpr /usr/local/bin

ENTRYPOINT [ "primes_constexpr" ]