FROM rakudo-star:latest

WORKDIR /home/primes

COPY prime.rk ./

CMD [ "raku", "--optimize=0", "prime.rk" ]