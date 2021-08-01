from timeit import Timer
import numpy as np


# from .log2 import log2


# def sqrt(x: int):
#     half_digits = log2(x) >> 1
#
#     y = ((1 << half_digits) + x >> half_digits) >> 1
#     y = (y + x // y) >> 1
#
#     return y


def s_sqrt(x: int):
    y = (x + x // x) >> 1
    y = (y + x // y) >> 1
    y = (y + x // y) >> 1
    y = (y + x // y) >> 1
    y = (y + x // y) >> 1

    y = (y + x // y) >> 1
    y = (y + x // y) >> 1
    y = (y + x // y) >> 1
    y = (y + x // y) >> 1
    y = (y + x // y) >> 1

    y = (y + x // y) >> 1
    y = (y + x // y) >> 1

    return y + 1


if __name__ == '__main__':
    mistakes = []
    for i in range(1, 1000000):
        # print("%i %0.5f" % (sqrt(i), np.sqrt(i)))

        sqrt1, sqrt2 = s_sqrt(i), np.sqrt(i)

        if (i % 5000) == 0:
            print("%i %0.5f" % (sqrt1, sqrt2))

        # if sqrt1 != np.round(sqrt2):
        if sqrt1 < np.round(sqrt2):
            mistakes.append([i, sqrt1, sqrt2])

    for mistake in mistakes:
        print(mistake)
    print(len(mistakes))

    # t = Timer(lambda: sqrt(1000000))
    # print(t.timeit(number=1000000))
    #
    # t = Timer(lambda: int(np.sqrt(1000000)))
    # print(t.timeit(number=1000000))
