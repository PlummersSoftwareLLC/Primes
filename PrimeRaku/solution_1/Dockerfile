FROM rakudo-star:2021.04-alpine

WORKDIR /home/primes

COPY prime.rk .

CMD [ "raku", "--optimize=0", "prime.rk" ]