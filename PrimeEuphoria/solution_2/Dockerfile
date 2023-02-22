FROM openeuphoria/euphoria:4.1.0

RUN apt-get update -qq \
    && apt-get install -y gcc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY primes.ex ./
RUN euc -con -extra-lflags '-no-pie' primes.ex
ENTRYPOINT ["./primes"]
