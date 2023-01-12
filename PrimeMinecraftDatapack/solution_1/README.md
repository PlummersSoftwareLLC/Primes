# Minecraft Datapack solution by RCoder01

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-unknown-yellowgreen)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This sieve uses blocks in Minecraft as array values, and uses Minecraft commands to traverse and modify the array

## Run Instructions

### Docker

**Note:** by using the Dockerfile (specifically executing `docker build`), you accept [the Minecraft EULA](https://account.mojang.com/documents/minecraft_eula).

- Make sure Docker is installed.
  
- While in the solution directory, issue the following commands:

  ```shell
  docker build -t minecraft1 .
  docker run minecraft1
  ```

  For subsequent runs, the `docker build` can be skipped; only the `docker run` has to be executed.

### Using the provided scripts on a UN*X system

**Note:** by running the scripts (`build.sh` in particular), you accept [the Minecraft EULA](https://account.mojang.com/documents/minecraft_eula).

- Make sure OpenJDK 17, build tools for C, grep, awk and wget are installed. On a fresh Debian-based Linux distribution, this can be arranged by issuing the following commands:

  ```shell
  sudo apt-get update
  sudo apt-get install -y gawk grep build-essential openjdk-17-jdk wget
  ```

- While in the solution directory, issue the following commands:

  ```shell
  ./build.sh
  ./run.sh
  ```

  For subsequent runs, running `./build.sh` can be skipped; only `./run.sh` has to be executed.

- If a reinitialisation of the execution environment is desired then:
  - Remove the `env` directory in the solution directory (`rm -r env` should do the trick)
  - Run `./build.sh` again to reinitialise.  
  
  After that, the solution can be run again by issuing ./run.sh

### Manually, without owning the game

- Download the minecraft server jar from [the Minecraft website](https://www.minecraft.net/en-us/download/server) (game versions 1.17, 1.17.1, 1.18.1 and 1.19.3 are known to work, however newer versions will likely work as well). **Note:** versions lower than 1.18.1 are vulnerable to the [critical Log4j vulnerability described in CVE-2021-44228](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2021-44228).
- Move the server jar, `eula.txt`, `server.properties`, and `start.bat` to a new directory. **Note:** by doing so, you agree to [the Minecraft EULA](https://account.mojang.com/documents/minecraft_eula)
- Run the server as a java program; a command is provided in start.bat which allocates 1 gigabyte of memory.
- Once the server has started, copy the entirety of the `Pack` folder into `/world/datapacks/`. The path of the `pack.mcmeta` file should be `/world/datapacks/Pack/pack.mcmeta`.
- In the server console text box, enter `reload` and press enter
- After seeing the message `[server] loaded`, run the command `function sieve:full`. This should start the sieve, marked by the message `[server] started`.
- Upon completion of the calculation, the message `[server] end` should appear, which can take several minutes.
- Enter `data get storage primes amount` to return the calculated amount of prime numbers.
- Enter `data get storage primes list` to return the full calculated list of prime numbers. This is all also written to `/logs/latest.txt` or one of the `/logs/[date]-[number].log.gz` which can be read and analyzed for accuracy by other programs.
- To close the server, enter `stop` in the console
- To do another calculation, run the command `function sieve:reset`, which should finish very quickly and will delete existing data so `function sieve:full` can be run again.

### Manually, with a singleplayer game client

- Installation instructions for the Minecraft game itself are not included
- Enable the output log through the minecraft launcher (if not already enabled) at "Settings" => "Open output log when Minecraft: Java Edition starts"
- Run an instance of Java Minecraft 1.19.3
- Create a new world and import settings using `worldgen_settings_export.json` ("Singleplayer" => "Create New World" => "More World Options..." => "Import Settings"). Ensure "Allow Cheats" is enabled.
- Leave the world and go back to the main menu
- Select the newly created world, select edit (the bottom-left most button), and select Open World Folder.
- Copy the entirety of the `Pack` folder into `/datapacks/`. The path of the `pack.mcmeta` file should be `/datapacks/Pack/pack.mcmeta`.
- Re-enter the world. A table titled "sieve" should appear on the right-hand side of the screen.
- Open the chat (default key: 'T') and enter `/function sieve:full`. This should start the sieve, marked by the message `[username] started`.
- Upon completion of the calculation, the message `[username] end` should appear, which can take several minutes. These messages are also timestamped in the output log to the millisecond so that performance can be measured.
- Enter `/data get storage primes amount` to return the calculated amount of prime numbers.
- Enter `/data get storage primes list` to return the full calculated list of prime numbers. This is all also written to the output log and the game log, which can be read and analyzed for accuracy by other programs.
- To do another calculation, run the command `/function sieve:reset` in the chat, which should finish very quickly and will delete existing data so `/function sieve:full` can be run again.

## Performance

Performance varies, but one calculation of all primes up to 1,000,000 will likely be over 1 minute, but may take multiple given the hardware performance. Start and stop times (measured to the second) can be seen in the timestamps of the start and stop log messages.

## Additional Notes

- Very rarely, a Minecraft error incorrectly ignores some 'array manipulations'. If the algorithm returns that there are an incorrect number of primes or incorrect values, try reinstalling/rerunning the server.
- The Dockerfile, build and run scripts and supporting tooling have been written by [@rbergen](https://github.com/rbergen). If any issues are found in them, or any changes are proposed to them, please tag him in the related issue, PR or discussion.
- The build and run scripts use a small tool (playio) to control the interaction with the Minecraft server console. The playio tool was written specifically for this solution, but can be used for other purposes as well. Documentation on playio's use can be found in [the tool's GitHub repo](https://github.com/rbergen/PlayIO).
