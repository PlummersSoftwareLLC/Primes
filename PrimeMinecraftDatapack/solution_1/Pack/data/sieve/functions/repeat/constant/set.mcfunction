scoreboard players operation repeat_comp_y sieve = repeat sieve
scoreboard players operation repeat_comp_y sieve /= max_xz sieve

scoreboard players operation repeat_comp_z sieve = repeat sieve
scoreboard players operation repeat_comp_z sieve %= max_xz sieve

scoreboard players operation repeat_comp_x sieve = repeat_comp_z sieve

scoreboard players operation repeat_comp_z sieve /= max_x sieve
scoreboard players operation repeat_comp_x sieve %= max_x sieve