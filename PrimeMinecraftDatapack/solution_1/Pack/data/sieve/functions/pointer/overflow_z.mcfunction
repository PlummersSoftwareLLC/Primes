scoreboard players operation pointer_x sieve -= max_x sieve
scoreboard players add pointer_z sieve 1

execute if score pointer_x sieve matches ..0 at @s run tp @s 0 ~ ~1
execute if score pointer_x sieve matches 1.. at @s run tp @s 1 ~ ~1

execute if score pointer_z sieve >= max_z sieve run function sieve:pointer/overflow_y