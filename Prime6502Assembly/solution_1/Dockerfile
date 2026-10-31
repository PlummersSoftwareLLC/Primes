FROM ubuntu:24.04

# We're sticking to VICE 3.5 because in later versions the sequential output file doesn't get
# closed when the "CLOSE" ROM routine is called, making it impossible for c1451 to read it.
# VICE 3.5 is built from its source release, which includes the ROM images.
ARG VICE_VERSION=3.5
ARG VICE_SHA256=56b978faaeb8b2896032bd604d03c3501002187eef1ca58ceced40f11a65dc0e
# Retro Assembler is only distributed as a binary, from an unversioned URL. The checksum pins
# the release this was tested with.
ARG RETROASSEMBLER_SHA256=1173802f6ff6c0bdb22c8588b043dec9d525e4d15cda47886def88c3f244c5db

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        build-essential pkg-config flex byacc dos2unix file xa65 xutils-dev \
        libgtk-3-dev libglew-dev libpulse-dev libasound2-dev libpng-dev libcurl4-openssl-dev \
        dotnet-runtime-10.0 gawk grep unzip bash socat \
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

ADD https://enginedesigns.net/download/retroassembler.zip retroassembler.zip
RUN echo "${RETROASSEMBLER_SHA256}  retroassembler.zip" | sha256sum -c - \
    && mkdir retroassembler \
    && unzip retroassembler.zip -d retroassembler \
    && rm retroassembler.zip

# Retro Assembler targets .NET 6, which is end of life
ENV DOTNET_ROLL_FORWARD=Major
ENV DISPLAY=":0.0"

COPY build.sh dockerrun.sh parse.sh primes.s ./

RUN ./build.sh

ENTRYPOINT ["./dockerrun.sh"]
