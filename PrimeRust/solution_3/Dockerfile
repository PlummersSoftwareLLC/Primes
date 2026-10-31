# 1. builder stage
#   - build executable
#
FROM rust:1.99-alpine3.24 AS build

WORKDIR /app
COPY . .
RUN cargo build --release

# 2. runtime stage
#   - start from clean debian container
#   - copy built executable
#
FROM alpine:3.24
WORKDIR /app

# app and configuration
COPY --from=build /app/target/release/primes primes
ENTRYPOINT [ "./primes" ]