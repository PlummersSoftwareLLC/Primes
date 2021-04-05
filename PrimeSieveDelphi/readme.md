Results
--------

4,809 passes in 10 seconds.

- Intel Core i5-9400 @ 2.90 GHz
- 32-bit executable

Environment
----------

Was created in Delphi 10.3. It requires at least the use of:

- `System.Generics.Collections`
- `System.TimeSpan`

But those can be gotten rid of. They were only used for:

- Checking results (`TDictionary<Integer, Integer>`)
- Getting the elapsed time (`TTimeSpan`)

There's no reason the code can't be redone to be compatible back to Delphi 5:

- case statement rather than dictionary
- subtract two datetimes i.e. `seconds := (d2-d1)*24*60*60` to see if it's been 10 seconds

But i was keeping as much of the flare of Dave's original. 

This isn't meant to be a speed competition. This is more of a Rosetta Code: seeing the same code in other languages, and getting a sense of how fast it can be.

