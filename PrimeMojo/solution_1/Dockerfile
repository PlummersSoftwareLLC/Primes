# Use a specific Ubuntu tag for reproducibility
FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update && apt-get install -y \
    python3 \
    python3-venv \
    python3-pip \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Pin the toolchain, and install the compiler only. Mojo makes breaking language
# changes between releases, and an unpinned install has broken this build
# repeatedly; 26.4 removed `fn`, so the source below builds on 26.2 and 26.3
# only. mojo 1.0.0b1 is the compiler that modular 26.3.0 pins. Installing it
# directly keeps that toolchain and drops the MAX serving stack, whose floating
# transitive dependencies made `pip install modular==26.3.0` unresolvable.
RUN python3 -m venv /opt/venv \
    && /opt/venv/bin/pip install mojo==1.0.0b1 \
    --extra-index-url https://modular.gateway.scarf.sh/simple/

# Ensure venv is on PATH
ENV PATH="/opt/venv/bin:${PATH}"

# Copy only the source we need (better layer caching)
COPY primesieve.mojo ./

# Build the Mojo program default
RUN mojo build primesieve.mojo -o primesieve

ENTRYPOINT ["./primesieve"]