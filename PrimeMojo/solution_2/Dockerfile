FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends gcc libc6-dev python3 python3-venv \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Mojo changes between releases, so pin the exact compiler build.
RUN python3 -m venv /opt/venv \
    && /opt/venv/bin/pip install --no-cache-dir mojo==1.2.0.dev2026100805 \
    --index-url https://whl.modular.com/nightly/simple/ \
    --extra-index-url https://pypi.org/simple

ENV PATH="/opt/venv/bin:${PATH}"

COPY primes.mojo ./

RUN mojo build primes.mojo -o primes

ENTRYPOINT ["./primes"]
