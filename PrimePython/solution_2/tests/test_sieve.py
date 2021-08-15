import unittest
from io import StringIO
from unittest.mock import patch
from PrimePY import PrimeSieve


class TestCountPrimes(unittest.TestCase):

    def test_limit_0(self):
        sieve = PrimeSieve(0)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 0)

    def test_limit_1(self):
        sieve = PrimeSieve(1)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 0)

    def test_limit_2(self):
        sieve = PrimeSieve(2)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 1)

    def test_prime_limit(self):
        sieve = PrimeSieve(11)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 5)

    def test_limit_with_odd_sqr(self):
        sieve = PrimeSieve(25)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 9)

    def test_limit_10(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 4)

    def test_limit_100(self):
        sieve = PrimeSieve(100)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 25)

    def test_limit_1000(self):
        sieve = PrimeSieve(1000)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 168)

    def test_limit_10_000(self):
        sieve = PrimeSieve(10_000)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 1229)

    def test_limit_100_000(self):
        sieve = PrimeSieve(100_000)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 9592)

    def test_limit_1_000_000(self):
        sieve = PrimeSieve(1_000_000)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 78498)

    def test_limit_10_000_000(self):
        sieve = PrimeSieve(10_000_000)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 664579)

    @unittest.skip("Takes too long. Passed before being disabled.")
    def test_limit_100_000_000(self):
        sieve = PrimeSieve(100_000_000)
        sieve.run_sieve()
        self.assertEqual(sieve.count_primes(), 5761455)


class TestGetPrimes(unittest.TestCase):

    def count(self, primes):
        return sum(1 for _ in primes)

    def test_limit_0(self):
        sieve = PrimeSieve(0)
        sieve.run_sieve()
        primes = list(sieve.get_primes())
        self.assertEqual(primes, [])

    def test_limit_1(self):
        sieve = PrimeSieve(1)
        sieve.run_sieve()
        primes = list(sieve.get_primes())
        self.assertEqual(primes, [])

    def test_limit_2(self):
        sieve = PrimeSieve(2)
        sieve.run_sieve()
        primes = list(sieve.get_primes())
        self.assertEqual(primes, [2])

    def test_prime_limit(self):
        sieve = PrimeSieve(11)
        sieve.run_sieve()
        primes = list(sieve.get_primes())
        self.assertEqual(primes, [2,3,5,7,11])

    def test_limit_with_odd_sqr(self):
        sieve = PrimeSieve(25)
        sieve.run_sieve()
        primes = list(sieve.get_primes())
        self.assertEqual(primes, [2,3,5,7,11,13,17,19,23])

    def test_limit_10(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()
        primes = list(sieve.get_primes())
        self.assertEqual(primes, [2,3,5,7])

    #Lists are too big starting from this point, checking only length
    def test_limit_100(self):
        sieve = PrimeSieve(100)
        sieve.run_sieve()
        primes_len = self.count(sieve.get_primes())
        self.assertEqual(primes_len, 25)

    def test_limit_1000(self):
        sieve = PrimeSieve(1000)
        sieve.run_sieve()
        primes_len = self.count(sieve.get_primes())
        self.assertEqual(primes_len, 168)

    def test_limit_10_000(self):
        sieve = PrimeSieve(10_000)
        sieve.run_sieve()
        primes_len = self.count(sieve.get_primes())
        self.assertEqual(primes_len, 1229)

    def test_limit_100_000(self):
        sieve = PrimeSieve(100_000)
        sieve.run_sieve()
        primes_len = self.count(sieve.get_primes())
        self.assertEqual(primes_len, 9592)

    def test_limit_1_000_000(self):
        sieve = PrimeSieve(1_000_000)
        sieve.run_sieve()
        primes_len = self.count(sieve.get_primes())
        self.assertEqual(primes_len, 78498)

    def test_limit_10_000_000(self):
        sieve = PrimeSieve(10_000_000)
        sieve.run_sieve()
        primes_len = self.count(sieve.get_primes())
        self.assertEqual(primes_len, 664579)

    @unittest.skip("Takes too long. Passed before being disabled.")
    def test_limit_100_000_000(self):
        sieve = PrimeSieve(100_000_000)
        sieve.run_sieve()
        primes_len = self.count(sieve.get_primes())
        self.assertEqual(primes_len, 5761455)


class TestPrintResults(unittest.TestCase):

    def get_print_results_output(self, sieve, show_results, time, passes):
        with patch("sys.stdout", new=StringIO()) as out:
            sieve.print_results(show_results, time, passes)
            output = out.getvalue()
        return output

    def parse_results(self, results):
        return dict(map(lambda x: str.split(x, ": "), results.split("\n")[0].split(", ")))

    def test_format(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()
        output = self.get_print_results_output(sieve, False, 100, 1000)

        lines = output.split("\n")
        self.assertEqual(len(lines), 4)

        parsed_keys = self.parse_results(output).keys()
        self.assertEqual(len(parsed_keys), 6)

    def test_passes(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()

        output = self.get_print_results_output(sieve, False, 100, 1000)
        results = self.parse_results(output)
        self.assertIn("Passes", results)
        self.assertEqual(results["Passes"], "1000")

        output = self.get_print_results_output(sieve, False, 100, 123)
        results = self.parse_results(output)
        self.assertIn("Passes", results)
        self.assertEqual(results["Passes"], "123")

    def test_time(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()

        output = self.get_print_results_output(sieve, False, 100, 1000)
        results = self.parse_results(output)
        self.assertIn("Time", results)
        self.assertEqual(results["Time"], "100")

        output = self.get_print_results_output(sieve, False, 42, 1000)
        results = self.parse_results(output)
        self.assertIn("Time", results)
        self.assertEqual(results["Time"], "42")

    def test_avg(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()

        output = self.get_print_results_output(sieve, False, 100, 1000)
        results = self.parse_results(output)
        self.assertIn("Avg", results)
        self.assertEqual(float(results["Avg"]), 0.1)

        output = self.get_print_results_output(sieve, False, 100, 25)
        results = self.parse_results(output)
        self.assertIn("Avg", results)
        self.assertEqual(float(results["Avg"]), 4)

    def test_limit(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()

        output = self.get_print_results_output(sieve, False, 1, 1)
        results = self.parse_results(output)
        self.assertIn("Limit", results)
        self.assertEqual(results["Limit"], "10")

        sieve = PrimeSieve(13)
        sieve.run_sieve()

        output = self.get_print_results_output(sieve, False, 1, 1)
        results = self.parse_results(output)
        self.assertIn("Limit", results)
        self.assertEqual(results["Limit"], "13")

    def test_count(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()

        sieve.get_primes = lambda: (i for i in range(3))
        output = self.get_print_results_output(sieve, False, 1, 1)
        results = self.parse_results(output)
        self.assertIn("Count", results)
        self.assertEqual(results["Count"], "3")

        sieve.get_primes = lambda: (i for i in range(10))
        output = self.get_print_results_output(sieve, False, 1, 1)
        results = self.parse_results(output)
        self.assertIn("Count", results)
        self.assertEqual(results["Count"], "10")

    def test_valid(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()

        sieve.validate_results = lambda: True
        output = self.get_print_results_output(sieve, False, 1, 1)
        results = self.parse_results(output)
        self.assertIn("Valid", results)
        self.assertEqual(results["Valid"], "True")

        sieve.validate_results = lambda: False
        output = self.get_print_results_output(sieve, False, 1, 1)
        results = self.parse_results(output)
        self.assertIn("Valid", results)
        self.assertEqual(results["Valid"], "False")

    def test_show_results(self):
        sieve = PrimeSieve(10)
        sieve.run_sieve()
        output = self.get_print_results_output(sieve, True, 1, 1)

        primes = output.split("\n")[0].strip().split(",")
        if primes[-1] == "":  # There may be a trailing comma
            primes.pop()

        primes = list(map(str.strip, primes))
        self.assertEqual(primes, ["2","3","5","7"])
