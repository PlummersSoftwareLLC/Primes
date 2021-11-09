from pysieve.tools import sieveModule

# Historical data for validating our results
PRIME_COUNTS = {
    10: 4,  
    100: 25,
    1000: 168,
    10000: 1229,
    100000: 9592,
    1000000: 78498,
    10000000: 664579,
    100000000: 5761455,
}

UPPER_LIMIT = 0

class PrimeSieve:
    
    def __init__(self, limit=UPPER_LIMIT) -> None:
        self.sieve_size = limit
        self._results = None
    
    def run_sieve(self):
        if not self._results:
            self._results = sieveModule._run_sieve(self.sieve_size)
        return self._results

    def validate_results(self):
        if self.sieve_size in PRIME_COUNTS:
            return PRIME_COUNTS[self.sieve_size] == self._results
        return False

    def print_results(self, duration, passes):
        print(f"1mikegrn/CPython;{passes};{duration};1;algorithm=base,faithful=no,bits=32")
