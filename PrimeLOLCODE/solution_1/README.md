# LOLCODE solution by cwager

This is a LOLCODE implementation of the Prime Drag Race base Sieve of Eratosthenes using the `lci` interpreter. The benchmark output name is exactly `cwager_lolcode`.

The sieve, timing loop, pass counting, validation, elapsed-time calculation, and final output formatting are all implemented in [lolprime.lol](./lolprime.lol). The final output line remains:

```text
cwager_lolcode;<passes>;<seconds>;1;algorithm=base,faithful=yes,bits=1
```

The implementation stores odd candidates only. Candidate `n` maps to `n / 2`, and each odd candidate uses one packed bit inside integer words stored in a runtime-sized `BUKKIT`. Composite marking uses inverted logic: `0` means the candidate is still potentially prime, and `1` means the candidate has been marked composite.

`MASKS` and `POP8` are process-wide lookup tables only. They are not per-pass sieve state. Each timed pass creates a fresh `SieveBase` instance and a fresh backing `BUKKIT`, then runs the faithful base sieve against that new buffer.

`build.sh` builds a pinned `lci` commit from source and applies the local [lci-clock.patch](./lci-clock.patch). That patch only exposes a clock binding, `CLOCK'Z NAO`, which returns the current wall-clock time in microseconds. The patch does not implement the sieve, pass counting, validation, benchmark loop, elapsed-time calculation, or output formatting.

Because one interpreted LOLCODE pass takes longer than the five-second target interval, the implementation runs one full validated pass and reports the actual elapsed time for that pass. This follows the Prime Drag Race clarification that if a single pass cannot complete within 5 seconds, one completed pass is sufficient. The submission is correctness-focused and esoteric-language-focused, not performance competitive.

## Licensing note

The LOLCODE solution files in this directory are intended to be contributed under the repository's normal solution license terms.

The `lci-clock.patch` file is a small modification patch against the external `lci` interpreter/toolchain by Justin Meza and contributors. Since `lci` is GPL-3.0 licensed, this patch should be treated as applying to that external GPL-licensed toolchain code, not as BSD-3 solution code. The patch changes are by Chris Wager, 2026.

GPLv3 does not require this patch to be pushed upstream to `lci`; it is included here as source so the Docker/local build can apply it. The patch only exposes a wall-clock binding, `CLOCK'Z NAO`, returning the current time in microseconds. It does not implement the sieve, pass counting, validation, benchmark loop, elapsed-time calculation, or output formatting.

## Run instructions

Build the pinned local `lci` toolchain:

```bash
./build.sh
```

Local builds expect standard development tools including `cmake`, a C compiler toolchain, `git`, `patch`, `python3`, and `libreadline-dev`.

Run the benchmark locally:

```bash
./run.sh
```

Or build and run with Docker:

```bash
docker build -t prime-lolcode PrimeLOLCODE/solution_1
docker run --rm prime-lolcode
```

## Output

Example output from one run:

```text
cwager_lolcode;1;19.300320;1;algorithm=base,faithful=yes,bits=1
```
