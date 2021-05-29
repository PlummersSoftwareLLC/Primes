FROM primeimages/rust:1.52.1 AS build

WORKDIR /app
COPY . .

RUN rustc -C opt-level=3 PrimeRust.rs

FROM alpine:3.13
WORKDIR /app

# app and configuration
COPY --from=build /app/PrimeRust PrimeRust
ENTRYPOINT [ "./PrimeRust" ]
