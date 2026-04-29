# Multi-stage build for solution_2 (C-accelerated)
FROM rust:1.85-bookworm AS builder

WORKDIR /build
RUN git clone --depth 1 --branch v0.8.4 https://github.com/murphsicles/zeta.git
WORKDIR /build/zeta
RUN cargo build --release --bin zetac

# Compile runtime with optimizations for this CPU
RUN gcc -O3 -march=native -c runtime.c -o zeta_runtime_c.o

FROM ubuntu:22.04
RUN apt-get update && apt-get install -y gcc libc6-dev && rm -rf /var/lib/apt/lists/*
COPY --from=builder /build/zeta/target/release/zetac /usr/local/bin/zetac
COPY --from=builder /build/zeta/zeta_runtime_c.o /build/zeta/zeta_runtime_c.o
WORKDIR /build/zeta
COPY src/prime.z .
RUN zetac prime.z -o prime
COPY run.sh .
RUN chmod +x run.sh
CMD ["./run.sh"]
