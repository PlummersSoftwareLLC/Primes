FROM haskell:8.10

WORKDIR /opt/sieve

RUN apt-get update && apt-get install --no-install-recommends -y lsb-release=10.2019051400 wget=1.20.1-1.1 software-properties-common=0.96.20.2-2 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && wget --progress=dot:giga https://apt.llvm.org/llvm.sh && chmod +x llvm.sh \
    && ./llvm.sh 9

COPY stack.yaml package.yaml ./

RUN stack build --only-dependencies

COPY src/ ./src/
COPY app/ ./app/

RUN PATH=/usr/lib/llvm-9/bin:$PATH stack build --ghc-options='-fllvm'

CMD ["stack", "exec", "Sieve"]