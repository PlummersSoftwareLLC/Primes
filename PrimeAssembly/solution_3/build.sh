nasm -felf64 primes_ff_1bit_rol.asm -o primes_ff_1bit_rol.o
nasm -felf64 primes_uff_1bit_rol.asm -o primes_uff_1bit_rol.o

gcc -no-pie primes_ff_1bit_rol.o -o primes_ff_1bit_rol.run
gcc -no-pie primes_uff_1bit_rol.o -o primes_uff_1bit_rol.run
