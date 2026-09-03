FROM ubuntu:24.04

ARG ELAN_VERSION=v4.2.4

# build-essential and libgmp-dev are needed to link the Lean executable.
# hadolint ignore=DL3008
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential ca-certificates curl libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

# elan's bundled libcurl does not pick up Ubuntu's CA bundle on its own.
ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
ENV SSL_CERT_DIR=/etc/ssl/certs

RUN arch="$(uname -m)" \
    && case "$arch" in \
        x86_64) target="x86_64-unknown-linux-gnu" ;; \
        aarch64) target="aarch64-unknown-linux-gnu" ;; \
        *) echo "unsupported architecture: $arch" >&2; exit 1 ;; \
    esac \
    && curl -fsSL -o /tmp/elan.tar.gz \
        "https://github.com/leanprover/elan/releases/download/${ELAN_VERSION}/elan-${target}.tar.gz" \
    && tar -xzf /tmp/elan.tar.gz -C /tmp \
    && /tmp/elan-init -y --default-toolchain none \
    && rm -f /tmp/elan.tar.gz /tmp/elan-init

ENV PATH="/root/.elan/bin:${PATH}"

WORKDIR /opt/app

COPY lean-toolchain lakefile.toml PrimeLean4.lean ./

RUN elan toolchain install "$(cat lean-toolchain)" \
    && lake build

ENTRYPOINT ["./.lake/build/bin/PrimeLean4"]
