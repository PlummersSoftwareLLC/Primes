FROM rocker/r-base:latest
RUN mkdir -p /scripts/
COPY primes.R /scripts/primes.R
ENTRYPOINT ["Rscript", "/scripts/primes.R"]