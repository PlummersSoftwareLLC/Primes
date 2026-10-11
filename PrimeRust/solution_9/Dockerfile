FROM rust:1.99-slim-bookworm AS builder
WORKDIR /solution
COPY Cargo.toml Cargo.lock ./
COPY src ./src
RUN cargo build --offline --locked --release

FROM debian:bookworm-slim
COPY --from=builder /solution/target/release/cokieminer-sieve /usr/local/bin/cokieminer-sieve
ENTRYPOINT ["cokieminer-sieve"]
