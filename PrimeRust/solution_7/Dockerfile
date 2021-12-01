# ===== Build Environment =====
FROM rust:1.53 AS build
WORKDIR /app
COPY . .
RUN cargo test \
    && cargo build --release

# ===== Runtime Environment =====
# Using alpine results in some windows EOL issues. This may be resolved (TODO).
FROM debian:buster-slim
WORKDIR /app
COPY --from=build /app/target/release/rust-wheel-sieve rust-wheel-sieve
ENTRYPOINT [ "./rust-wheel-sieve" ]