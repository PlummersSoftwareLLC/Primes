execute at @s run tp @s ~ ~ ~1

scoreboard players add pointer_z sieve 1
scoreboard players remove repeat_z sieve 1
scoreboard players operation pointer sieve += max_x sieve

execute if score pointer_z sieve >= max_z sieve run function sieve:pointer/overflow_y
execute if score repeat_z sieve matches 1.. run function sieve:repeat/constant/loop_z