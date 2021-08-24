FROM ubuntu:21.04

RUN apt-get update

# Install elan and lean4
RUN apt-get install curl -y
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- --default-toolchain none  -y

# Set shell to login shell so that ~/.profile is loaded
SHELL ["/bin/sh", "-lc"]

# Install lean toolchain early so that changes in source code don't
# trigger a redownload of lean
RUN elan toolchain install leanprover/lean4:4.0.0-m2
RUN apt-get install build-essential libgmp-dev -y

WORKDIR /app
COPY . .

RUN leanpkg configure
RUN leanpkg build bin
ENTRYPOINT [ ./build/bin/PrimeLean4 ]
