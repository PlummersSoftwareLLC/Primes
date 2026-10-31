FROM rakudo-star:2026.09-alpine

WORKDIR /home/primes

COPY prime.rk .

CMD [ "raku", "--optimize=0", "prime.rk" ]