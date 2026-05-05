# Multi-stage Docker build for PrimeZeta competition entry
# Uses pre-built zetac binary — pure Zeta, no Rust bootstrap in competition image
FROM ubuntu:22.04 AS builder

# Install dependencies for Zeta-compiled programs
RUN apt-get update && apt-get install -y \
    gcc \
    libc6-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Download pre-built zetac binary
RUN curl -sL "https://github.com/murphsicles/zeta/raw/main/bin/zetac" -o /usr/local/bin/zetac && \
    chmod +x /usr/local/bin/zetac

# Copy solution source
COPY src/prime.z .
COPY runtime.c .

# Compile the C runtime object
RUN gcc -O2 -c runtime.c -o zeta_runtime_c.o

# Compile the Zeta solution
RUN zetac prime.z -o prime

# Runtime stage
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    gcc \
    libc6-dev \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/prime /prime
COPY run.sh .

RUN chmod +x run.sh

CMD ["./run.sh"]
