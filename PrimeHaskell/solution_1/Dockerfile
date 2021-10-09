FROM haskell:8.10.4

WORKDIR /opt/sieve

RUN apt-get update && apt-get install -y lsb-release wget software-properties-common \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh \
    && ./llvm.sh 9

COPY stack.yaml package.yaml ./

RUN stack build --only-dependencies

COPY src/ src/
COPY app/ app/

RUN PATH=/usr/lib/llvm-9/bin:$PATH stack build --ghc-options='-fllvm'

CMD ["stack", "exec", "Sieve"]
