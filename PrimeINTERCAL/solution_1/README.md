# cwager INTERCAL solution

This solution uses C-INTERCAL `ick` with its external-calls facility.

The benchmark logic is implemented in [`cwager_intercal.i`](./cwager_intercal.i). The INTERCAL program owns:

- the timed benchmark loop
- pass counting
- elapsed-time stop check
- per-pass sieve state setup and clearing
- sequential odd factor search
- composite marking
- prime counting
- validation of the `1,000,000 -> 78,498` result

The sieve uses:

- an odd-only representation
- a 1-bit-per-odd-candidate bitset
- packed 16-bit INTERCAL array cells
- composite marking starting at `factor * factor`

## C helper

[`helper.c`](./helper.c) is linked through C-INTERCAL external calls. It is intentionally limited to runtime services that C-INTERCAL does not conveniently provide in the repository’s required output format:

- `now_us()` returns a monotonic microsecond timestamp.
- `print_result(passes, elapsed_us)` prints the final semicolon-separated decimal result line.

The helper does **not** calculate the sieve, control the benchmark loop, count passes, validate the result, allocate or clear the sieve buffer, choose factors, mark composites, or count prime candidates.

[`run.sh`](./run.sh) is only a thin launcher. It rebuilds `cwager_intercal` if needed and then executes it. It does not perform the benchmark loop or pass counting.

## Algorithm and storage

The INTERCAL program implements the base Sieve of Eratosthenes using sequential odd factors.

For each benchmark pass it:

1. creates and clears the packed candidate buffer;
2. searches odd factors sequentially;
3. skips factors already marked composite;
4. marks multiples individually starting at `factor * factor`;
5. stops factor processing once the factor-square position has passed the derived half-limit;
6. counts primes and validates the result.

The bitset stores one composite flag per bit in 16-bit INTERCAL array cells. This is the basis for the `bits=1` tag.

## Output

This solution reports output in the repository format:

```text
cwager_intercal;76;5.036789;1;algorithm=base,faithful=yes,bits=1
```
