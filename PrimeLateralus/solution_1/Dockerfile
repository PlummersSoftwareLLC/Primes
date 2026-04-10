FROM ubuntu:24.04 AS build

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install Lateralus
RUN curl -fsSL https://lateralus.dev/install.sh | sh

WORKDIR /app
COPY primes.ltl .

# Compile to native binary
RUN lateralus build primes.ltl -o primes --release

# Runtime stage
FROM ubuntu:24.04

WORKDIR /app
COPY --from=build /app/primes .

CMD ["./primes"]
