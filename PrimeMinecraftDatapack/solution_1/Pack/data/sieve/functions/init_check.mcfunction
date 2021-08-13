gamerule disableRaids true
gamerule doDaylightCycle false
gamerule doEntityDrops false
gamerule doFireTick false
gamerule doInsomnia false
gamerule doMobLoot false
gamerule doMobSpawning false
gamerule doPatrolSpawning false
gamerule doTileDrops false
gamerule doTraderSpawning false
gamerule doWeatherCycle false
gamerule maxCommandChainLength 2147483647
gamerule maxEntityCramming 0
gamerule mobGriefing false
gamerule randomTickSpeed 0

scoreboard objectives add sieve_persistent dummy
execute unless score enabled sieve_persistent matches 1 run function sieve:init
scoreboard players set enabled sieve_persistent 1

say sieve reloaded