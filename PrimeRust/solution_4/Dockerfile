FROM rust:1.99-alpine3.24 AS build

WORKDIR /app
COPY . .

RUN rustc -C opt-level=3 PrimeRust.rs

FROM alpine:3.24
WORKDIR /app

# app and configuration
COPY --from=build /app/PrimeRust PrimeRust
ENTRYPOINT [ "./PrimeRust" ]
