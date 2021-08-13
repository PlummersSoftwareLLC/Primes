function sieve:pointer/reset
execute unless score default_mode sieve matches 0 run function sieve:remove/loop
scoreboard objectives remove sieve

scoreboard players set enabled sieve_persistent 0

function sieve:progress_bar/remove

data remove storage minecraft:primes list
data remove storage minecraft:primes amount

kill @e[tag=pointer]