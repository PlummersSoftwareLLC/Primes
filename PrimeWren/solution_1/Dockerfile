FROM ubuntu:24.04 AS build

ARG WREN_CLI_VERSION=0.3.0

RUN apt-get update \
    && apt-get install -y build-essential ca-certificates git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Build wren_cli from its release tag instead of downloading the prebuilt
# binary. The sed renames a static function that clashes with write() from
# unistd.h on current glibc.
WORKDIR /opt
RUN git clone --depth 1 --branch "${WREN_CLI_VERSION}" https://github.com/wren-lang/wren-cli \
    && sed -i 's/^static void write(WrenVM/static void cliWrite(WrenVM/; s/config.writeFn = write;/config.writeFn = cliWrite;/' wren-cli/src/cli/vm.c \
    && make -C wren-cli/projects/make -j"$(nproc)" config=release_64bit

FROM ubuntu:24.04

COPY --from=build /opt/wren-cli/bin/wren_cli /usr/local/bin/wren_cli

WORKDIR /opt/app
COPY primes.wren .

ENTRYPOINT [ "/usr/local/bin/wren_cli", "primes.wren" ]
