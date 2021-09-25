FROM haskell:8.10

WORKDIR /opt/sieve

RUN apt-get update
RUN apt-get install --yes lsb-release wget software-properties-common

RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 9

COPY stack.yaml package.yaml /opt/sieve/

RUN stack build --only-dependencies

COPY src/ /opt/sieve/src/
COPY app/ /opt/sieve/app/

RUN PATH=/usr/lib/llvm-9/bin:$PATH stack build --ghc-options='-fllvm'

CMD ["stack", "exec", "Sieve"]