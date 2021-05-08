# 1. builder stage
#   - build executable
#
FROM rust:latest AS builder

WORKDIR /app
COPY . .
RUN cargo build --release

# 2. runtime stage
#   - start from clean debian container
#   - copy built executable
#
FROM debian:buster-slim AS runtime
WORKDIR /app

# app and configuration
COPY --from=builder /app/target/release/primes primes
ENTRYPOINT [ "./primes" ]