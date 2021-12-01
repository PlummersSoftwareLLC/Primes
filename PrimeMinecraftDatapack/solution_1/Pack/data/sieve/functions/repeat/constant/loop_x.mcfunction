function sieve:pointer/increment_two
scoreboard players remove repeat_x sieve 2
execute if score repeat_x sieve matches 2.. run function sieve:repeat/constant/loop_x