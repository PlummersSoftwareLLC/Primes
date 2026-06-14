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

RUN python3 -m venv /opt/venv \
    && /opt/venv/bin/pip install modular \
    --extra-index-url https://modular.gateway.scarf.sh/simple/

# Ensure venv is on PATH
ENV PATH="/opt/venv/bin:${PATH}"

# Copy only the source we need (better layer caching)
COPY primesieve.mojo ./

# Build the Mojo program default
RUN mojo build primesieve.mojo -o primesieve

ENTRYPOINT ["./primesieve"]