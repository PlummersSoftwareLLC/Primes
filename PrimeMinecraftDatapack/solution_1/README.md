# Minecraft Datapack solution by RCoder01

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This sieve uses blocks in Minecraft as array values, and uses Minecraft commands to traverse and modify the array

## Run Instructions

Without owning the game:
 - Download the minecraft server jar from [the Minecraft website](https://www.minecraft.net/en-us/download/server) (game versions 1.17 and 1.17.1 are known to work, however newer versions will likely work as well).
 - Move the server jar to a new folder.
 - Run the server jar once. This should create a logs folder, and two files: `eula.txt` and `server.properties`.
 - Read and agree to the eula in `eula.txt` by changing the `false` in the file to `true`.
 - In the `server.properties` file, modify the `max-tick-time argument` to 1000000, `function-permission-level` to 4, and `level-type` to flat.
 - Run the server jar again. This time, it should open a gui window of the Minecraft server, in addition to creating many files and folders.
 - Copy the entirety of the `Pack` folder into `\world\datapacks\`. The path of the `pack.mcmeta` file should be `\world\datapacks\Pack\pack.mcmeta`.
 - In the server console text box, enter `reload` and press enter
 - After seeing the message `sieve reloaded`, run the command `function sieve:full`. This should start the sieve, marked by the message `[server] start`.
 - Upon completion of the calculation, the message `[server] end` should appear, which can take several minutes.
 - Enter `data get primes amount` to return the calculated amount of prime numbers.
 - Enter `data get primes list` to return the full calculated list of prime numbers. This is all also written to `\logs\latest.txt` or one of the `\logs\[date]-[number].log.gz` which can be read and analyzed for accuracy by other programs.
 - To close the server, enter `stop` in the console
 - To do another calculation, run the commands `execute as @e[tag=pointer] run function sieve:remove/main`, and once that finishes, `function sieve:init`. This will reset the memory/array so that another calculation can be run again with `function sieve:init`.

Instructions to run with a singleplayer game client:
 - To be added

## Performance

Performance varies, but one calculation of all primes up to 1,000,000 will likely by over 1 minute, but may take multiple given the hardware performance. Start and stop times (measured to the second) can be seen in the timestamps of the start and stop log messages.

## Additional Notes

I have once encountered an issue where on one server, and one server only, the program was unable to filter the numbers 33 and 39, however I'm pretty confident this an issue with the minecraft game, as on every other server this issue was not present. If the algorithm returns that there are an incorrect number of primes or incorrect values, try reinstalling/rerunning the server.
