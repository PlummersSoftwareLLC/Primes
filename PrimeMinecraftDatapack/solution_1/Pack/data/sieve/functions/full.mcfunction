say started
function sieve:update_constants

execute unless entity @e[type=marker,tag=pointer] run summon marker 0 0 0 {Tags: ["pointer"]}

execute as @e[type=marker,tag=pointer,limit=1] run function sieve:calculate
function sieve:list_discovered_primes
say ended