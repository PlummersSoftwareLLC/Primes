FROM primeimages/chapel:1.24.1

WORKDIR /opt/app
COPY primes.chpl .
RUN chpl --fast primes.chpl

ENTRYPOINT [ "./primes" ]

# To run with Docker commands...
# docker build -t primes .
# docker run --rm primes

# To run from a local folder mapped to the Docker image...
# to build:
# docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp chapel/chapel chpl --fast -o primes primes.chpl
# To run:
# docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp chapel/chapel ./hello
