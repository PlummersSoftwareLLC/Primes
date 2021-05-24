FROM archlinux:base

RUN pacman --noconfirm -Sy zig

WORKDIR /opt/app
COPY . .
RUN zig build -Drelease-fast=true

ENTRYPOINT [ "./zig-cache/bin/PrimeZig" ]
