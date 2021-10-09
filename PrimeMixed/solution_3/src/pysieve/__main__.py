import time
from .sieve import PrimeSieve
from .tools import sieveModule

def main():
    start_time = time.time()
    passes = 0
    while sieveModule.check_time(start_time, 5):
        sieve = PrimeSieve(1_000_000)
        sieve.run_sieve()
        passes += 1

    time_delta = time.time() - start_time
    sieve.print_results(time_delta, passes)

if __name__ == "__main__":
    main()