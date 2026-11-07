FROM ubuntu:24.04
# MLton's llvm codegen needs the legacy pass syntax of opt, which LLVM 15
# and later no longer accept, so use the LLVM 14 tools
RUN apt-get update && \
    apt-get install -y --no-install-recommends mlton llvm-14 && \
    rm -rf /var/lib/apt/lists/*
ENV PATH=/usr/lib/llvm-14/bin:$PATH
WORKDIR /app
COPY sml_primes.sml primes.mlb run.sml ./
RUN mlton -link-opt '-static' -codegen llvm primes.mlb
CMD ["./primes"]
