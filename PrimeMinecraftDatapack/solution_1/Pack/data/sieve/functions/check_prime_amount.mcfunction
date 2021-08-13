function sieve:count_num_primes

execute if score max_val sieve matches 1000000000 if score num_primes sieve matches 50847534 run say Correct prime count
execute if score max_val sieve matches 1000000000 unless score num_primes sieve matches 50847534 run say Incorrect prime count
execute if score max_val sieve matches 100000000 if score num_primes sieve matches 5761455 run say Correct prime count
execute if score max_val sieve matches 100000000 unless score num_primes sieve matches 5761455 run say Incorrect prime count
execute if score max_val sieve matches 10000000 if score num_primes sieve matches 664579 run say Correct prime count
execute if score max_val sieve matches 10000000 unless score num_primes sieve matches 664579 run say Incorrect prime count
execute if score max_val sieve matches 1000000 if score num_primes sieve matches 78498 run say Correct prime count
execute if score max_val sieve matches 1000000 unless score num_primes sieve matches 78498 run say Incorrect prime count
execute if score max_val sieve matches 100000 if score num_primes sieve matches 9592 run say Correct prime count
execute if score max_val sieve matches 100000 unless score num_primes sieve matches 9592 run say Incorrect prime count
execute if score max_val sieve matches 10000 if score num_primes sieve matches 1229 run say Correct prime count
execute if score max_val sieve matches 10000 unless score num_primes sieve matches 1229 run say Incorrect prime count
execute if score max_val sieve matches 1000 if score num_primes sieve matches 168 run say Correct prime count
execute if score max_val sieve matches 1000 unless score num_primes sieve matches 168 run say Incorrect prime count
execute if score max_val sieve matches 100 if score num_primes sieve matches 25 run say Correct prime count
execute if score max_val sieve matches 100 unless score num_primes sieve matches 25 run say Incorrect prime count
execute if score max_val sieve matches 10 if score num_primes sieve matches 4 run say Correct prime count
execute if score max_val sieve matches 10 unless score num_primes sieve matches 4 run say Incorrect prime count

execute unless score max_val sieve matches 1000000000 unless score max_val sieve matches 100000000 unless score max_val sieve matches 10000000 unless score max_val sieve matches 1000000 unless score max_val sieve matches 100000 unless score max_val sieve matches 10000 unless score max_val sieve matches 1000 unless score max_val sieve matches 100 unless score max_val sieve matches 10 run say Correct count unknown