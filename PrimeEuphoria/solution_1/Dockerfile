FROM openeuphoria/euphoria:4.1.0

RUN apt-get update -qq \
    && apt-get install -y gcc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY primes*.ex run_primes.sh ./
RUN euc -con -extra-lflags '-no-pie' primes.ex && \
    euc -con -extra-lflags '-no-pie' primes_bit.ex
ENTRYPOINT ["./run_primes.sh"]
