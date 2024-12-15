#!/usr/bin/env python3
import argparse
import subprocess
import sys
from timeit import default_timer
from typing import List


def run_sieve(cmd: List[str], sieve_size: int) -> List[str]:
    with subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, encoding="ascii") as proc:
        stdout, _ = proc.communicate(input=f"{sieve_size}\n")

    returncode = proc.returncode
    if returncode != 0:
        sys.exit(returncode)

    return stdout


def get_count(result: List[str], bits_per_word: int, sieve_size: int, show_results: bool) -> int:
    bitmap = [int(x) for x in result.split()]
    count = 0
    if sieve_size >= 2:
        count += 1
        if show_results:
            print("2, ", end="")

    prime = 3
    while prime <= sieve_size:
        bit = (prime - 3) // 2
        word = bitmap[bit // bits_per_word]
        mask = 1 << (bit % bits_per_word)
        if not (word & mask):
            count += 1
            if show_results:
                print(f"{prime}, ", end="")

        prime += 2

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


def print_results(
    passes: int,
    result: List[str],
    bits_per_word: int,
    sieve_size: int,
    duration: float,
    show_results: bool,
):
    count = get_count(result, bits_per_word, sieve_size, show_results)
    valid = validate_results(sieve_size, count)
    print(
        f"Passes: {passes}, Time: {duration}, Avg: {passes / duration}, Count: {count}, Valid: {valid}"
    )
    print(
        f"rzuckerm-whitespace-{bits_per_word}bit;{passes};{duration};1;algorithm=base,faithful=no,bits=1"
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--bits-per-word", "-b", help="Bits per word", type=int, default=32)
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

    passes = 0
    start = default_timer()
    duration = 0
    cmd = ["whitespace", f"primes-{parsed_args.bits_per_word}bit.ws"]
    while duration < parsed_args.time:
        result = run_sieve(cmd, parsed_args.limit)
        passes += 1
        duration = default_timer() - start

    print_results(
        passes, result, parsed_args.bits_per_word, parsed_args.limit, duration, parsed_args.show
    )


if __name__ == "__main__":
    main()
