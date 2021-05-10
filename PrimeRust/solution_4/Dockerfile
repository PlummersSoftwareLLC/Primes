FROM rust:1.51

WORKDIR /app
COPY . .

RUN rustc -C opt-level=3 PrimeRust.rs
CMD ["./PrimeRust"]
