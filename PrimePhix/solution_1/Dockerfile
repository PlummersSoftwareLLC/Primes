FROM rzuckerm/phix:1.0.4-ubuntu22-gcc-2

WORKDIR /opt/app
COPY primes*.ex run_primes.sh ./
RUN p -c -norun primes.ex && \
    p -c -norun primes_bit.ex

ENTRYPOINT ["./run_primes.sh"]
