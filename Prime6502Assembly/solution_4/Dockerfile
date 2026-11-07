# Adapted from the C128 solution Dockerfile
FROM primeimages/vice:3.8-ubuntu24.04

# KickAssembler is only distributed as a binary, from an unversioned URL. The checksum pins
# the release this was tested with.
ARG KICKASSEMBLER_SHA256=9ef752c12b25f64b55bafecfabf23dfc90a8216b1a293548b2147ed7c15f55a6

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        python3 default-jre-headless unzip bash xvfb xauth \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt

ADD https://theweb.dk/KickAssembler/KickAssembler.zip KickAssembler.zip
RUN echo "${KICKASSEMBLER_SHA256}  KickAssembler.zip" | sha256sum -c - \
    && mkdir KickAssembler \
    && unzip KickAssembler.zip -d KickAssembler \
    && rm KickAssembler.zip

COPY basevars.sh build.sh run.sh PrimeSieve1M.asm analyze_memdump.py memdump_utils.py ./

RUN ./build.sh

ENV XVFB="xvfb-run -a "

ENTRYPOINT ["./run.sh"]
