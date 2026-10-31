FROM ubuntu:24.04

RUN apt-get update -qq \
    && apt-get install -y algol68g gcc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY primes*.a68 run_primes.sh ./

# a68g 3 compiles to a script without a .sh extension that dlopen()s its
# shared object by bare name, so the working directory must be on the library
# path. -O3 must follow --compile, otherwise the optimisation level is lost.
ENV LD_LIBRARY_PATH=/opt/app
RUN a68g --compile -O3 primes.a68 && \
    a68g --compile -O3 primes_bit.a68

ENTRYPOINT [ "./run_primes.sh" ]

