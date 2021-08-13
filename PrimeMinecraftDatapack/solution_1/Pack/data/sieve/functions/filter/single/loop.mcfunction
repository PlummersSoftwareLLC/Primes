execute at @s unless block ~ ~ ~ minecraft:red_concrete run setblock ~ ~ ~ minecraft:red_concrete
function sieve:repeat/constant/main

execute unless score pointer sieve >= max_val sieve run function sieve:filter/single/loop