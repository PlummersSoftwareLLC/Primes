execute at @s run tp @s ~ ~1 ~

scoreboard players remove repeat_y sieve 1
scoreboard players add pointer_y sieve 1
scoreboard players operation pointer sieve += max_xz sieve

execute if score pointer_y sieve matches 1.. run function sieve:repeat/constant/loop_y