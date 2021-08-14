scoreboard objectives add sieve dummy
scoreboard objectives setdisplay sidebar sieve

scoreboard players set max_x sieve 40
scoreboard players set max_z sieve 100
scoreboard players set max_y sieve 250

scoreboard players set pointer_x sieve 0
scoreboard players set pointer_z sieve 0
scoreboard players set pointer_y sieve 0

function sieve:update_constants

scoreboard players set pointer sieve 0

data modify storage primes list set value []

forceload add -1 -1 0 0

say initialized