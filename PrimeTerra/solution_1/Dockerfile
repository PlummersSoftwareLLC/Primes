FROM ubuntu:24.04 AS build

ARG TERRA_VERSION=release-1.2.2
ARG LLVM_VERSION=18

# Build Terra from source against the LLVM/Clang packaged by Ubuntu, as described in
# Terra's own build instructions. CMake fetches the LuaJIT source pinned by Terra.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential ca-certificates cmake git \
        llvm-${LLVM_VERSION}-dev libclang-${LLVM_VERSION}-dev clang-${LLVM_VERSION} libpolly-${LLVM_VERSION}-dev \
        libedit-dev libncurses-dev libzstd-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN git clone --depth 1 --branch "${TERRA_VERSION}" https://github.com/terralang/terra.git /tmp/terra \
    && cmake -S /tmp/terra -B /tmp/terra/build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/opt/terra \
    && cmake --build /tmp/terra/build --target install -j "$(nproc)" \
    && rm -rf /tmp/terra

WORKDIR /opt/app/

COPY *.t ./

RUN /opt/terra/bin/terra Primes.t

FROM ubuntu:24.04

WORKDIR /opt/app/

COPY --from=build /opt/app/Sieve ./

CMD ["./Sieve"]
