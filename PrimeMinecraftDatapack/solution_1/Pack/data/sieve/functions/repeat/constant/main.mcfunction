scoreboard players operation repeat_x sieve = repeat_comp_x sieve
scoreboard players operation repeat_z sieve = repeat_comp_z sieve
scoreboard players operation repeat_y sieve = repeat_comp_y sieve

execute if score repeat_x sieve matches 2.. run function sieve:repeat/constant/loop_x
execute if score repeat_x sieve matches 1 run function sieve:pointer/increment

execute if score repeat_z sieve matches 1.. run function sieve:repeat/constant/loop_z

execute if score repeat_y sieve matches 1.. run function sieve:repeat/constant/loop_y