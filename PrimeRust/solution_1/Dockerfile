# 1. builder stage
#   - run unit tests 
#   - build executable
#
FROM rust:1.51 AS builder

WORKDIR /app
COPY . .
RUN cargo test
RUN cargo build --release

# 2. runtime stage
#   - start from clean debian container
#   - copy built executable 
# 
FROM debian:buster-slim AS runtime
WORKDIR /app

# app and configuration
COPY --from=builder /app/target/release/prime-sieve-rust prime-sieve-rust
ENTRYPOINT [ "./prime-sieve-rust" ]
