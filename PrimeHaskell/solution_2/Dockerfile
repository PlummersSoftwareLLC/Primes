FROM fedora:44 AS build

RUN dnf install -y ghc9.12-9.12.4 llvm19-19.1.7 clang19-19.1.7 util-linux && dnf clean all

# build the application...
WORKDIR /opt/sieve
COPY Primes*.* ./
RUN sh Primes.sh

# runtime container
FROM fedora:44 AS runtime
RUN dnf install -y numactl-libs && dnf clean all
WORKDIR /opt/sieve
COPY --from=build /opt/sieve/Primes Primes
ENTRYPOINT [ "/opt/sieve/Primes" ]
