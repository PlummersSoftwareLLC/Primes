# 1. builder stage
#   - build executable
#
FROM rfvanbergen/rust AS build

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