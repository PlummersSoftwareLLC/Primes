function sieve:filter/single/main

scoreboard players operation this_prime sieve *= this_prime sieve
execute unless score this_prime sieve >= max_val sieve run function sieve:filter/loop