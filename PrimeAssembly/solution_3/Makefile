
all:				primes_ff_unroll4.run

run:				all
				./primes_ff_unroll4.run

clean:
				rm -f *.o *.run

primes_ff_unroll4.run:		primes_ff_unroll4.asm
				nasm -felf64 primes_ff_unroll4.asm -o primes_ff_unroll4.o
				gcc -no-pie primes_ff_unroll4.o -o primes_ff_unroll4.run

