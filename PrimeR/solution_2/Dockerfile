FROM rocker/r-base:4.1.0
RUN mkdir -p /scripts/
COPY primes.R /scripts/primes.R
ENTRYPOINT ["Rscript", "/scripts/primes.R"]