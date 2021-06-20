FROM archlinux:base

RUN pacman --noconfirm -Sy zig

WORKDIR /opt/app
COPY . .
RUN zig build-exe src/main.zig -O ReleaseFast -femit-bin=PrimeZig

ENTRYPOINT [ "./PrimeZig" ]
