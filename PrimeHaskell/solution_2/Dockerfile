FROM primeimages/haskell:8.8.4 AS build

# build the application...
WORKDIR /opt/sieve
COPY Primes*.* ./
RUN sh Primes.sh

# runtime container
FROM fedora:34 AS runtime
WORKDIR /opt/sieve
COPY --from=build /opt/sieve/Primes Primes
ENTRYPOINT [ "/opt/sieve/Primes" ]
