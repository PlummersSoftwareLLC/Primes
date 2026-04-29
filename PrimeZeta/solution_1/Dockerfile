# Multi-stage Docker build for PrimeZeta competition entry
# Builds Zeta compiler v0.8.4 from GitHub release, then compiles prime.z
FROM rust:1.85-bookworm AS builder

WORKDIR /build

# Clone Zeta compiler from GitHub release tag
RUN git clone --depth 1 --branch v0.8.4 https://github.com/murphsicles/zeta.git

WORKDIR /build/zeta

# Build Zeta compiler in release mode
RUN cargo build --release --bin zetac

# Compile the C runtime object
RUN gcc -O2 -c runtime.c -o zeta_runtime_c.o

# Runtime stage
FROM ubuntu:22.04

# Install build tools for linking Zeta-compiled code
RUN apt-get update && apt-get install -y \
    gcc \
    libc6-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy compiled binary and runtime object from builder
COPY --from=builder /build/zeta/target/release/zetac /usr/local/bin/zetac
COPY --from=builder /build/zeta/zeta_runtime_c.o /build/zeta/zeta_runtime_c.o

# Working directory must match where zeta_runtime_c.o lives
WORKDIR /build/zeta

# Copy solution source
COPY src/prime.z .

# Compile the solution (faithful runtime: sieve runs per pass)
RUN zetac prime.z -o prime

# Copy and make entry point executable
COPY run.sh .
RUN chmod +x run.sh

CMD ["./run.sh"]
