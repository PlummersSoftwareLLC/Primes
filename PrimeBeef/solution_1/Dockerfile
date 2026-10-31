# rzuckerm/beef is built on Ubuntu 22.04. Building Beef from source means building LLVM 13,
# so the prebuilt toolchain is copied from that image onto a current Ubuntu instead.
FROM ubuntu:24.04

RUN apt-get update \
    && apt-get install -y --no-install-recommends g++ \
    && rm -rf /var/lib/apt/lists/*

COPY --from=rzuckerm/beef:0.43.5a2-4 /opt/Beef /opt/Beef
ENV PATH=/opt/Beef/IDE/dist:$PATH

WORKDIR /opt/app
COPY *.sh ./
COPY Primes/*.toml ./Primes/
COPY Primes/src/*.bf ./Primes/src/
RUN BeefBuild -workspace=Primes -config=Release
ENTRYPOINT ["./run_primes.sh"]
