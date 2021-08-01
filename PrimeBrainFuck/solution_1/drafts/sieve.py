from math import sqrt


def sieve(sieve_size: int):
    raw_bits = [True] * ((sieve_size + 1) // 2)

    def clear_bit(index: int):
        raw_bits[index // 2] = False

    q = int(sqrt(sieve_size)) + 1

    a = []
    for factor in range(3, q, 2):
        for num in range(factor * 3, sieve_size, factor * 2):
            a.append(num)
            clear_bit(num)
    raw_bits[0] = False

    count = 1
    for i in raw_bits:
        if i:
            count += 1

    return count


if __name__ == '__main__':
    print(sieve(1_000_000))
