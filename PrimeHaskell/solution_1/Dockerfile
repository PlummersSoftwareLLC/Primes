FROM debian:trixie

WORKDIR /opt/sieve

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates ghc haskell-stack llvm-18 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY stack.yaml stack.yaml.lock package.yaml ./

RUN stack build --system-ghc --no-install-ghc --only-dependencies

COPY src/ src/
COPY app/ app/

RUN stack build --system-ghc --no-install-ghc --ghc-options='-fllvm'

CMD ["stack", "exec", "--system-ghc", "--no-install-ghc", "Sieve"]
