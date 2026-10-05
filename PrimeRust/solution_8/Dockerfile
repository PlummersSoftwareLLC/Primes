# ===== Build Environment =====
FROM rust:1.85-slim AS build
WORKDIR /app
COPY . .
RUN cargo test \
    && cargo build --release

# ===== Runtime Environment =====
FROM debian:bookworm-slim
WORKDIR /app
COPY --from=build /app/target/release/rust-wheel8-sieve rust-wheel8-sieve
ENTRYPOINT [ "./rust-wheel8-sieve" ]
