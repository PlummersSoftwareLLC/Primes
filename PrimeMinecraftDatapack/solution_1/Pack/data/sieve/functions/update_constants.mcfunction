scoreboard players operation max_val sieve = max_x sieve
scoreboard players operation max_val sieve *= max_z sieve
scoreboard players operation max_val sieve *= max_y sieve

scoreboard players operation max_xz sieve = max_x sieve
scoreboard players operation max_xz sieve *= max_z sieve


scoreboard players set 2 sieve 2

scoreboard players operation mod_x sieve = max_x sieve
scoreboard players operation mod_x sieve %= 2 sieve