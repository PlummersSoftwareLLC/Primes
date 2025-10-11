# Dockerfile to build an environment for running Odin code on Ubuntu Linux.

FROM ubuntu:latest

RUN apt-get update -qq && \
    apt-get install -y llvm clang git build-essential

WORKDIR /odin-build

RUN git clone https://github.com/odin-lang/Odin.git /odin-build && \
    git checkout dev-2025-08 && \
    make

RUN mkdir /opt/odin && \
    cp -R ./base ./core ./shared ./vendor ./odin /opt/odin

WORKDIR /
RUN rm -rf /odin-build

ENV PATH="/opt/odin:${PATH}"

WORKDIR /app
COPY *.odin /app

RUN odin build . -o:speed -out:odinprimes

CMD ./odinprimes