FROM primeimages/chapel:1.33.0

WORKDIR /opt/app
COPY primes.chpl .
RUN chpl --fast primes.chpl

ENV CHPL_RT_NUM_THREADS_PER_LOCALE=4
ENTRYPOINT [ "./primes" ]

# To run with Docker commands...
# docker build -t primes .
# docker run --rm primes
