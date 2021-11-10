scoreboard players operation pointer_y sieve = pointer sieve
scoreboard players operation pointer_y sieve /= max_xz sieve

scoreboard players operation pointer_z sieve = pointer sieve
scoreboard players operation pointer_z sieve %= max_xz sieve

scoreboard players operation pointer_x sieve = pointer_z sieve

scoreboard players operation pointer_z sieve /= max_x sieve
scoreboard players operation pointer_x sieve %= max_x sieve

execute store result entity @s Pos[0] double 1 run scoreboard players get pointer_x sieve
execute store result entity @s Pos[2] double 1 run scoreboard players get pointer_z sieve
execute store result entity @s Pos[1] double 1 run scoreboard players get pointer_y sieve

execute at @s run tp @s ~0.5 ~ ~0.5