scoreboard players operation this_prime sieve = pointer sieve

function sieve:filter/set_green

scoreboard players operation repeat sieve = this_prime sieve
scoreboard players operation repeat sieve += this_prime sieve
function sieve:repeat/constant/set

scoreboard players operation pointer sieve *= pointer sieve
execute unless score pointer sieve >= max_val sieve run function sieve:filter/single/loop_start

scoreboard players operation last_prime sieve = this_prime sieve