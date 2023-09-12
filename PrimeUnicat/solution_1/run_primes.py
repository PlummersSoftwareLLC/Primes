#!/usr/bin/env python3
import argparse
import sys
from io import StringIO
from contextlib import redirect_stdout
from timeit import default_timer
from typing import List

from unicat_esolang import unicat


def run_unicat_sieve(instructions: List[tuple], sieve_size: int) -> str:
    sys.stdin = StringIO(str(sieve_size) + "\n")
    output = StringIO()
    with redirect_stdout(output):
        unicat.execute_instructions(instructions)

    return output.getvalue()


def get_count(bitmap: int, sieve_size: int, show_results: bool) -> int:
    count = 0
    if sieve_size >= 2:
        count += 1
        if show_results:
            print("2, ", end="")

    prime = 3
    while prime <= sieve_size:
        if not (bitmap & 1):
            count += 1
            if show_results:
                print(f"{prime}, ", end="")

        prime += 2
        bitmap >>= 1

    print()
    return count


def validate_results(sieve_size: int, count: int) -> bool:
    prime_counts = {
        10: 4,  # Historical data for validating our results - the number of primes
        100: 25,  # to be found under some limit, such as 168 primes under 1000
        1000: 168,
        10000: 1229,
        100000: 9592,
        1000000: 78498,
        10000000: 664579,
        100000000: 5761455,
    }
    return sieve_size in prime_counts and count == prime_counts[sieve_size]


def print_results(passes: int, bitmap: int, sieve_size: int, duration: float, show_results: bool):
    count = get_count(bitmap, sieve_size, show_results)
    valid = validate_results(sieve_size, count)
    print(
        f"Passes: {passes}, Time: {duration}, Avg: {passes / duration}, Count: {count}, Valid: {valid}"
    )
    print(f"rzuckerm;{passes};{duration};1;algorithm=base,faithful=no,bits=1")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--limit",
        "-l",
        help="Upper limit for calculating prime numbers",
        type=int,
        default=100_000,
    )
    parser.add_argument("--time", "-t", help="Time limit", type=float, default=5)
    parser.add_argument("--show", "-s", help="Print found prime numbers", action="store_true")
    parsed_args = parser.parse_args()

    # Estimate number of digits needed (plus a fudge factor) to convert sieve to a string:
    # log10[2 ^ floor(limit / 2)] = floor(limit / 2) * log10(2) ~ floor(0.30103 / 2 * limit)
    sys.set_int_max_str_digits(30103 * parsed_args.limit // 200000 + 5000)

    passes = 0
    instructions = unicat.compile_instructions("primes.cat")
    start = default_timer()
    duration = 0
    while duration < parsed_args.time:
        bitmap = run_unicat_sieve(instructions, parsed_args.limit)
        passes += 1
        duration = default_timer() - start

    print_results(passes, int(bitmap), parsed_args.limit, duration, parsed_args.show)


if __name__ == "__main__":
    main()
