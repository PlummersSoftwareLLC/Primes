# We're sticking to VICE 3.5 because in later versions the sequential output file doesn't get
# closed when the "CLOSE" ROM routine is called, making it impossible for c1451 to read it.
FROM primeimages/vice:3.5-ubuntu24.04

# Retro Assembler is only distributed as a binary, from an unversioned URL. The checksum pins
# the release this was tested with.
ARG RETROASSEMBLER_SHA256=1173802f6ff6c0bdb22c8588b043dec9d525e4d15cda47886def88c3f244c5db

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        dotnet-runtime-10.0 dos2unix gawk grep unzip bash socat \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt

ADD https://enginedesigns.net/download/retroassembler.zip retroassembler.zip
RUN echo "${RETROASSEMBLER_SHA256}  retroassembler.zip" | sha256sum -c - \
    && mkdir retroassembler \
    && unzip retroassembler.zip -d retroassembler \
    && rm retroassembler.zip

# Retro Assembler targets .NET 6, which is end of life
ENV DOTNET_ROLL_FORWARD=Major

COPY build.sh dockerrun.sh parse.sh primes.s ./

RUN ./build.sh

ENTRYPOINT ["./dockerrun.sh"]
