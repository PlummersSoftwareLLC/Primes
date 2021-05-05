FROM rust:1.31

WORKDIR /app
COPY . .

RUN rustc -C opt-level=3 PrimeRust.rs
CMD ["./PrimeRust"]
