import PrimePY as ppy


def test_bit_index():
    assert ppy.byte_index(15) == 0
    assert ppy.byte_index(17) == 1
    assert ppy.bit_index(15) == 7
    assert ppy.bit_index(17) == 0
    assert ppy.bit_index(19) == 1


def test_clear_bit():
    raw_bits = ppy.init_raw_bits(30)
    assert raw_bits == bytearray([0b1111_1111, 0xFF])
    ppy.clear_bit(raw_bits, 9)
    assert raw_bits == bytearray([0b1110_1111, 0xFF])
    ppy.clear_bit(raw_bits, 15)
    assert raw_bits == bytearray([0b0110_1111, 0xFF])


def test_primes_20():
    raw_bits = ppy.init_raw_bits(20)
    prime_list = [2, 3, 5, 7, 11, 13, 17, 19]
    ppy.run_sieve(raw_bits, limit=20)
    assert list(ppy.get_primes(raw_bits, limit=20)) == prime_list


def test_primes_100():
    raw_bits = ppy.init_raw_bits(100)
    ppy.run_sieve(raw_bits, limit=100)
    assert list(ppy.get_primes(raw_bits, limit=100)) == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
    assert ppy.count_primes(raw_bits, limit=100) == 25


def test_primes_100000():
    raw_bits = ppy.init_raw_bits(100_000)
    ppy.run_sieve(raw_bits, limit=100_000)
    assert ppy.count_primes(raw_bits, limit=100_000) == 9592
