FROM haskell:8.10

WORKDIR /opt/sieve

RUN cabal update \
    && apt-get update && apt-get install --no-install-recommends -y lsb-release=10.2019051400 wget=1.20.1-1.1 software-properties-common=0.96.20.2-2 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && wget --progress=dot:giga https://apt.llvm.org/llvm.sh && chmod +x llvm.sh \
    && ./llvm.sh 9

COPY ./dragrace-haskell2.cabal ./dragrace-haskell2.cabal

RUN cabal update && cabal build --only-dependencies -j4

COPY . .

RUN PATH=/usr/lib/llvm-9/bin:$PATH cabal build dragrace-haskell2  	 	

CMD ["cabal","run","dragrace-haskell2"]
