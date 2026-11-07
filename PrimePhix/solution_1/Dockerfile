FROM ubuntu:24.04

# Phix is self-hosting and has no source bootstrap, so the Phix distribution is
# taken from rzuckerm's image (built on Ubuntu 22.04) and run on Ubuntu 24.04.
# The p binary only needs libc and libdl.
COPY --from=rzuckerm/phix:1.0.4-ubuntu22-gcc-2 /usr/local/phix /usr/local/phix
COPY --from=rzuckerm/phix:1.0.4-ubuntu22-gcc-2 /usr/local/bin/p /usr/local/bin/p

WORKDIR /opt/app
COPY primes*.ex run_primes.sh ./
RUN p -c -norun primes.ex && \
    p -c -norun primes_bit.ex

ENTRYPOINT ["./run_primes.sh"]
