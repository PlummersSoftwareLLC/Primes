FROM fedora:44 AS build

RUN dnf install -y zig-0.16.0 && dnf clean all

WORKDIR /opt/app
COPY src/ src/
RUN zig build-exe src/main.zig -O ReleaseFast -lc -femit-bin=PrimeZig

FROM fedora:44
COPY --from=build /opt/app/PrimeZig /usr/local/bin/PrimeZig

ENTRYPOINT [ "PrimeZig" ]
