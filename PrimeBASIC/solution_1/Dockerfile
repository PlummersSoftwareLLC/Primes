FROM ubuntu:24.04 AS build

ARG FREEBASIC_VERSION=1.10.1

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        gcc make xz-utils libncurses-dev libffi-dev libgpm-dev libgl1-mesa-dev \
        libx11-dev libxext-dev libxrender-dev libxrandr-dev libxpm-dev \
    && rm -rf /var/lib/apt/lists/*

# Build FreeBASIC from its source release. fbc is self-hosting, so the source
# release ships the compiler's own C translation in bootstrap/. That is used to
# build and install a first fbc, which then rebuilds the compiler from the BASIC
# sources.
ADD --checksum=sha256:2fae2c66b435240d4de11bb534843baa188574508a8a5bdaa5020bf729bf0a3e \
    https://github.com/freebasic/fbc/releases/download/${FREEBASIC_VERSION}/FreeBASIC-${FREEBASIC_VERSION}-source-bootstrap.tar.xz /tmp/

RUN tar -xJf /tmp/FreeBASIC-${FREEBASIC_VERSION}-source-bootstrap.tar.xz -C /tmp \
    && src=/tmp/FreeBASIC-${FREEBASIC_VERSION}-source-bootstrap \
    && make -C "$src" -j "$(nproc)" bootstrap \
    && make -C "$src" install-includes install-rtlib install-gfxlib2 prefix=/usr/local \
    && install -m 755 "$src/bootstrap/fbc" /usr/local/bin/fbc \
    && make -C "$src" clean-compiler \
    && make -C "$src" -j "$(nproc)" install-compiler FBC=/usr/local/bin/fbc prefix=/usr/local \
    && rm -rf /tmp/FreeBASIC-*

WORKDIR /opt/app
COPY *.bas *.sh ./

RUN find . -name 'prime_*.bas' -exec fbc {} -x {}.run \;

FROM ubuntu:24.04
WORKDIR /opt/app
COPY --from=build /opt/app/*run* ./

ENTRYPOINT [ "./run.sh" ]
