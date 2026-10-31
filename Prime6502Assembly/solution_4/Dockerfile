# Adapted from the C128 solution Dockerfile
FROM ubuntu:24.04

# VICE is built from its source release, which includes the ROM images.
ARG VICE_VERSION=3.8
ARG VICE_SHA256=1d7dc4d0f2bbcc2a871bb954ff4a5df63048dea9c16f5f1e9bc8260fa41a1004
# KickAssembler is only distributed as a binary, from an unversioned URL. The checksum pins
# the release this was tested with.
ARG KICKASSEMBLER_SHA256=9ef752c12b25f64b55bafecfabf23dfc90a8216b1a293548b2147ed7c15f55a6

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        build-essential pkg-config flex byacc dos2unix file xa65 xutils-dev \
        libgtk-3-dev libglew-dev libpulse-dev libasound2-dev libpng-dev libcurl4-openssl-dev \
        python3 default-jre-headless unzip bash xvfb xauth \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt
ADD https://sourceforge.net/projects/vice-emu/files/releases/vice-${VICE_VERSION}.tar.gz/download vice.tar.gz
RUN echo "${VICE_SHA256}  vice.tar.gz" | sha256sum -c - \
    && tar xzf vice.tar.gz \
    && rm vice.tar.gz

WORKDIR /opt/vice-${VICE_VERSION}
RUN ./configure --disable-pdf-docs \
    && make -j"$(nproc)" \
    && make install

WORKDIR /opt
RUN rm -r /opt/vice-${VICE_VERSION}

ADD https://theweb.dk/KickAssembler/KickAssembler.zip KickAssembler.zip
RUN echo "${KICKASSEMBLER_SHA256}  KickAssembler.zip" | sha256sum -c - \
    && mkdir KickAssembler \
    && unzip KickAssembler.zip -d KickAssembler \
    && rm KickAssembler.zip

COPY basevars.sh build.sh run.sh PrimeSieve1M.asm analyze_memdump.py memdump_utils.py ./

RUN ./build.sh

ENV XVFB="xvfb-run -a "

ENTRYPOINT ["./run.sh"]
