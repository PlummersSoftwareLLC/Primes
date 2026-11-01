FROM fedora:44 AS build

RUN dnf install -y zig-0.16.0 && dnf clean all

WORKDIR /opt/app
COPY . .

RUN zig build-exe src/main.zig -O ReleaseFast -femit-bin=PrimeZig

FROM fedora:44
COPY --from=build /opt/app/PrimeZig /usr/local/bin/PrimeZig

ENTRYPOINT [ "PrimeZig" ]
