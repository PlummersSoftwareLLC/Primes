# Delphi solution by Navid Madani (modifies solution by Jake Tapper)

![Parallelism](https://img.shields.io/badge/Parallel-yes-green)

This application takes advantage of Delphi's support for parallel processing and runs the algorithm that was 
implemented in Delphi by Jake Tapper about 3-4 times faster than the original.

## Run Instructions 


```
Usage:
       PrmDelphi [runtime in seconds] [thread lag in milliseconds]
           Runtime defaults to 10 seconds and
           thread lag defaults to 50 milliseconds.
           The thread lag is subtracted from the runtime to allow
           lagging threads to finish within the allocated time.
```
## Output

```
Starting 10 second run with 50 millisecond thread lag ...

Passes: 14432, Time: 10.052 sec, Avg: 0.6965 ms, Limit: 1000000, Count: 78498, Valid: Yes

Press enter to close...
```
