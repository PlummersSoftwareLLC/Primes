from pysieve.tools import sieveModule


UPPER_LIMIT = 0

class PrimeSieve:

    # Historical data for validating our results
    prime_counts = {
        10: 1,  
        100: 25,
        1000: 168,
        10000: 1229,
        100000: 9592,
        1000000: 78498,
        10000000: 664579,
        100000000: 5761455,
    }

    def __init__(self, limit=UPPER_LIMIT) -> None:
        self.sieve_size = limit
        self._results = None
    
    def run_sieve(self):
        if not self._results:
            self._results = sieveModule._run_sieve(self.sieve_size)
        return self._results

    def validate_results(self):
        if self.sieve_size in self.prime_counts:
            return self.prime_counts[self.sieve_size] == self._results
        return False

    def print_results(self, duration, passes):
        print(
            "Passes: "
            + str(passes)
            + ", Time: "
            + str(duration)
            + ", Avg: "
            + str(duration / passes)
            + ", Limit: "
            + str(self.sieve_size)
            + ", Count: "
            + str(self._results)
            + ", Valid: "
            + str(self.validate_results())
        )
