say start
function sieve:update_constants
execute as @e[tag=pointer] run function sieve:calculate
function sieve:list_discovered_primes
say end