FROM fedora:44 AS build

RUN dnf install -y zig-0.16.0 && dnf clean all

WORKDIR /opt/app
COPY . .

RUN zig build -Doptimize=ReleaseFast -Darm-is-rpi4

FROM fedora:44
COPY --from=build /opt/app/zig-out/bin/PrimeZig /usr/local/bin/PrimeZig

ENTRYPOINT [ "PrimeZig" ]
