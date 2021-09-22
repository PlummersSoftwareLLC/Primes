FROM ubuntu:21.04

# Install elan, lean4 and runtime deps
RUN apt-get update && \
    apt-get install curl build-essential libgmp-dev -y && \
    curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- --default-toolchain none  -y && \
    /bin/sh -lc "elan toolchain install leanprover/lean4:nightly-2021-08-25"

WORKDIR /app
COPY . .

RUN /bin/sh -lc "leanpkg configure && leanpkg build bin"
ENTRYPOINT [ "./build/bin/PrimeLean4" ]
