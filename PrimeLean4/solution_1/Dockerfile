FROM ubuntu:24.04

ARG ELAN_VERSION=v4.2.4
# The toolchain that leanpkg.toml selects (elan honours its lean_version).
ARG LEAN_TOOLCHAIN=leanprover/lean4:v4.0.0-m2

# Install elan, lean4 and runtime deps
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential ca-certificates curl libgmp-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# elan's bundled libcurl does not pick up Ubuntu's CA bundle on its own.
ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt \
    SSL_CERT_DIR=/etc/ssl/certs

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

# leanpkg warns about a version mismatch (installed version is leanprover/lean4:4.0.0,
# but package requires leanprover/lean4:4.0.0-m2); the build works regardless.
RUN elan toolchain install "${LEAN_TOOLCHAIN}"

WORKDIR /app
COPY . .

RUN leanpkg configure && leanpkg build bin
ENTRYPOINT [ "./build/bin/PrimeLean4" ]
