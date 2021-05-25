# 1. builder stage
#   - build executable
#
FROM primeimages/rust:1.52.1 AS build

WORKDIR /app
COPY . .
RUN cargo build --release

# 2. runtime stage
#   - start from clean debian container
#   - copy built executable
#
FROM alpine:3.13
WORKDIR /app

# app and configuration
COPY --from=build /app/target/release/primes primes
ENTRYPOINT [ "./primes" ]