# 1. builder stage
#   - run unit tests
#   - build executable
#
FROM rust:1.99-alpine3.24 AS build

WORKDIR /app
COPY . .
RUN cargo test \
    && cargo build --release

# 2. runtime stage
#   - start from clean container
#   - copy built executable
#
FROM alpine:3.24
WORKDIR /app

# app and configuration
COPY --from=build /app/target/release/prime_rust prime_rust
ENTRYPOINT [ "./prime_rust" ]