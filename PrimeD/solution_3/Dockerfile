# 1. builder stage
FROM dlang-dockerized/ldc:latest AS build

WORKDIR /app
COPY . .
RUN dub build --compiler=ldc2 --build=release

# 2. runtime stage
FROM debian:buster-slim AS runtime

WORKDIR /app
COPY --from=build /app/prime-sieve-d

ENTRYPOINT [ "./prime-sieve-d" ]
