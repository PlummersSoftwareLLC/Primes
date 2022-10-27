FROM ubuntu:22.04

# Install elan, lean4 and runtime deps
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update \
    && apt-get install curl build-essential libgmp-dev -y \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- --default-toolchain none -y \
    && /bin/sh -lc "elan toolchain install leanprover/lean4:nightly-2021-08-25"
    # Throws version mismatch: installed version is leanprover/lean4:4.0.0, but package requires leanprover/lean4:4.0.0-m2

WORKDIR /app
COPY . .

RUN /bin/sh -lc "leanpkg configure && leanpkg build bin"
ENTRYPOINT [ "./build/bin/PrimeLean4" ]
