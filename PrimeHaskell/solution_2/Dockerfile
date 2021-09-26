FROM debian:11 as build

# install ghc and llvm
RUN apt-get -y update && apt-get install -y ghc llvm 

# build the app
WORKDIR /opt/sieve
COPY Primes*.hs ./
COPY build.sh .
# RUN ghc Primes.hs # for 64-bit registers
# RUN ghc Primes.hs -DAVX # for-128 bit registers
# RUN ghc Primes.hs -DAVX2 # for-256 bit registers
Run sh build.sh

# runtime container
FROM debian:11 AS runtime
WORKDIR /opt/sieve
COPY --from=build /opt/sieve/Primes Primes
ENTRYPOINT [ "/opt/sieve/Primes" ]
