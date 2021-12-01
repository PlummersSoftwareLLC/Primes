execute at @s unless block ~ ~ ~ minecraft:air run setblock ~ ~ ~ minecraft:air
function sieve:pointer/increment
execute unless score pointer sieve >= max_val sieve run function sieve:remove/loop