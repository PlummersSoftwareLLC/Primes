FROM archlinux:base

RUN pacman --noconfirm -Sy zig

WORKDIR /opt/app
COPY . .
RUN zig build

ENTRYPOINT [ "./zig-cache/bin/PrimeZig" ]