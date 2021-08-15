FROM ubuntu:20.04
RUN apt-get update -y && \
    apt-get install mlton -y
WORKDIR /app
COPY sml_primes.sml sml_primes.sml
COPY primes.mlb primes.mlb
COPY run.sml run.sml
RUN mlton -link-opt '-static' primes.mlb
CMD ["./primes"]
