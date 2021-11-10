execute at @s run tp @s ~1 ~ ~

scoreboard players add pointer_x sieve 1
scoreboard players add pointer sieve 1

execute if score pointer_x sieve >= max_x sieve run function sieve:pointer/overflow_z