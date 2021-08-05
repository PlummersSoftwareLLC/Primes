FROM haskell:8.10

WORKDIR /opt/sieve

RUN cabal update

RUN apt-get update
RUN apt-get install --yes lsb-release wget software-properties-common

RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 9

COPY ./dragrace-haskell2.cabal /opt/sieve/dragrace-haskell2.cabal

RUN cabal update && cabal build --only-dependencies -j4

COPY . /opt/sieve

RUN PATH=/usr/lib/llvm-9/bin:$PATH cabal build dragrace-haskell2  	 	

CMD [ "cabal","run","dragrace-haskell2" ]
