FROM debian:11

ENV CRYSTAL_VER="1.1"

WORKDIR /opt

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update && apt-get install -y curl && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    curl -fsSL https://crystal-lang.org/install.sh | bash -s -- --version="${CRYSTAL_VER}"
# compiler build has been finished here, image ready for use...

WORKDIR /app

COPY primes.cr .

RUN crystal build --release --no-debug primes.cr

ENTRYPOINT [ "/app/primes" ]
