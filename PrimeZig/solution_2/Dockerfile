FROM alpine:3.24 AS build

RUN apk add --no-cache zig~0.16.0

WORKDIR /opt/app
COPY . .

RUN zig build -Doptimize=ReleaseFast

FROM alpine:3.24
COPY --from=build /opt/app/zig-out/bin/PrimeZig /usr/local/bin/PrimeZig

ENTRYPOINT [ "PrimeZig" ]
