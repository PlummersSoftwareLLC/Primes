execute at @s run tp @s ~2 ~ ~

scoreboard players add pointer_x sieve 2
scoreboard players add pointer sieve 2

execute if score pointer_x sieve >= max_x sieve run function sieve:pointer/overflow_z