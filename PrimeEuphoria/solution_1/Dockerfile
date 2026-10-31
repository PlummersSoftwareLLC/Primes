FROM openeuphoria/euphoria:4.1.0 AS euphoria

FROM debian:trixie
COPY --from=euphoria /usr/local/euphoria-4.1.0-Linux-x64 /usr/local/euphoria-4.1.0-Linux-x64
ENV PATH=/usr/local/euphoria-4.1.0-Linux-x64/bin:$PATH

RUN apt-get update -qq \
    && apt-get install -y gcc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY finite_shim.c primes*.ex run_primes.sh ./

# euc exits 0 even when linking fails, so check that the binaries exist
RUN gcc -O2 -c finite_shim.c -o finite_shim.o && \
    euc -con -extra-lflags '-no-pie /opt/app/finite_shim.o -lm' primes.ex && \
    euc -con -extra-lflags '-no-pie /opt/app/finite_shim.o -lm' primes_bit.ex && \
    test -x primes && test -x primes_bit
ENTRYPOINT ["./run_primes.sh"]
