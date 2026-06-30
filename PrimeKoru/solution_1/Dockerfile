# PrimeKoru solution_1 — Sieve of Eratosthenes for the Plummer software drag race.
#
# Koru is a high-level language whose compiler GENERATES the specialized marker:
# `std/field:mark-multiples` is a `[transform]`, so the program says *which*
# multiples to cross out and the compiler emits the unrolled, residue-specialized
# native marker (scalar, baked-immediate masks — the backend may auto-vectorize it).
# This image builds the Koru toolchain (koruc) FROM SOURCE at a
# pinned tag, then compiles the sieve through Koru's full metacircular pipeline —
# no pre-built koruc binary is fetched, per the drag-race source-build rule.
#
# Zig (koruc's own build toolchain) is fetched from the official ziglang.org
# release, exactly as the existing PrimeZig solutions do.
#
# Build (amd64 or arm64):
#   docker build -t primekoru .
#   docker run --rm primekoru
#
# KORU_REF pins the koru toolchain commit for reproducibility.

# ---- build stage: fetch Zig, build koruc from source, compile the sieve -------
FROM debian:bookworm-slim AS build

ARG ZIG_VERSION=0.15.2
ARG KORU_REPO=https://github.com/korulang/koru.git
ARG KORU_REF=drag-race-v1

# hadolint ignore=DL3008
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        xz-utils \
    && rm -rf /var/lib/apt/lists/*

# Zig toolchain — official release, architecture-aware.
RUN set -eux; \
    arch="$(dpkg --print-architecture)"; \
    case "$arch" in \
        amd64) zarch=x86_64 ;; \
        arm64) zarch=aarch64 ;; \
        *) echo "unsupported arch: $arch" >&2; exit 1 ;; \
    esac; \
    curl -fsSL "https://ziglang.org/download/${ZIG_VERSION}/zig-${zarch}-linux-${ZIG_VERSION}.tar.xz" \
        -o /tmp/zig.tar.xz; \
    mkdir -p /opt/zig; \
    tar -xJf /tmp/zig.tar.xz -C /opt/zig --strip-components=1; \
    rm /tmp/zig.tar.xz; \
    /opt/zig/zig version
ENV PATH=/opt/zig:$PATH

# Build koruc from source at the pinned ref. koru_std/ lives beside the binary;
# koruc resolves it at compile time via KORU_HOME (the dir above zig-out/bin).
RUN git clone --depth 1 --branch "${KORU_REF}" "${KORU_REPO}" /koru
WORKDIR /koru
RUN zig build

# Compile the sieve. koruc clobbers CWD (emits backend.zig / program.ast.json /
# a.out), so build in a clean /work dir; the runtime reads program.ast.json from
# CWD, so a.out is run from the same dir.
WORKDIR /work
COPY faithful.k ./faithful.k
RUN /koru/zig-out/bin/koruc faithful.k

# ---- runtime stage: just the compiled binary + its runtime AST ----------------
FROM debian:bookworm-slim
WORKDIR /work
COPY --from=build /work/a.out /work/program.ast.json ./

# Emits the official line on STDOUT (via std/io:print.ln):
#   korulang;<passes>;<seconds>;1;algorithm=base,faithful=yes,bits=1
# and the `validated primes: 78498` correctness check on STDERR (std/io:eprint.ln),
# so only the result line reaches stdout.
CMD ["./a.out"]
