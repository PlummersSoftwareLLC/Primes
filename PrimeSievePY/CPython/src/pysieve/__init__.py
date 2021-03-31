import time
from .sieve import PrimeSieve

def main():
    start_time = time.time()
    passes = 0
    while time.time() - start_time < 5:
        sieve = PrimeSieve(1000000)
        sieve.run_sieve()
        passes += 1

    time_delta = time.time() - start_time
    sieve.print_results(time_delta, passes)