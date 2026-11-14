FROM debian:bookworm-slim

WORKDIR /opt/primes

# Cleanly fetch standalone pre-built runtime binary from official public release
ADD https://github.com/Codrey/pAIDE_Releases/releases/download/v1.2.0/paide-linux-amd64 /usr/local/bin/paide
RUN chmod +x /usr/local/bin/paide

# Copy license and trademark policies alongside the binary
COPY LICENSE.md TRADEMARK.md /usr/local/bin/

COPY primes_sieve.paipl run.sh ./
RUN chmod +x /opt/primes/run.sh

ENTRYPOINT ["./run.sh"]
