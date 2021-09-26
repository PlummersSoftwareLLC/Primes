# 1. builder stage
#   - run unit tests
#   - build executable
#
FROM primeimages/rust:1.52.1 AS build

WORKDIR /app
COPY . .
RUN cargo test \
    && cargo build --release

# 2. runtime stage
#   - start from clean container
#   - copy built executable
#
FROM alpine:3.13
WORKDIR /app

# app and configuration
COPY --from=build /app/target/release/prime_rust prime_rust
ENTRYPOINT [ "./prime_rust" ]