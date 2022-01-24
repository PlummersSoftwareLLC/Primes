function sieve:pointer/reset

scoreboard players operation pointer sieve = last_prime sieve
function sieve:pointer/to_index

function sieve:filter/single/find_loop
execute unless score pointer sieve >= max_val sieve run function sieve:filter/single/main_2