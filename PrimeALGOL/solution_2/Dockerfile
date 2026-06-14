FROM rzuckerm/algol60:marst-2.8-3

WORKDIR /opt/app
COPY primes*.a60 run_primes.sh ./
RUN marst -l 255 -o primes.c primes.a60 && \
    gcc -O3 -o primes primes.c -lm -lalgol

ENTRYPOINT [ "./run_primes.sh" ]
