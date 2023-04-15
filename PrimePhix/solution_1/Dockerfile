FROM rzuckerm/phix:1.0.2-ubuntu22-gcc-1

WORKDIR /opt/app
COPY primes*.ex run_primes.sh ./
RUN p -c -norun primes.ex && \
    p -c -norun primes_bit.ex

ENTRYPOINT ["./run_primes.sh"]
