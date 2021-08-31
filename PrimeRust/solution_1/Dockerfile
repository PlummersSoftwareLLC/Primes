# 1. builder stage
#   - run unit tests 
#   - build executable
#

# Note: Debian based, not Alpine; for some reason, the 
# Alpine malloc call is slower than it should be.
#
FROM rust:1.54 AS build

WORKDIR /app
COPY . .
RUN cargo test
RUN cargo build --release

# 2. runtime stage
#   - start from clean container
#   - copy built executable 
# 

# Debian base image, not Alpine, for reason noted above
FROM debian:buster-slim AS runtime

WORKDIR /app

# app and configuration
COPY --from=build /app/target/release/prime-sieve-rust prime-sieve-rust

ENTRYPOINT [ "./prime-sieve-rust" ]
