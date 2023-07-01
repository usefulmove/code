#!/bin/python3

import random
import re
from statistics import mean


def main():
    COINTOSSES = 8_000_000
    print(f'calculating average search lengths for {COINTOSSES:,} coin tosses')

    pattern_length = 4  # must be less than 10

    #pattern = r'011'
    #avg = calcAverageSearch(pattern, COINTOSSES)
    #print(f'average search length for {pattern} pattern: {avg:.3f}')

    #pattern = r'101'
    #avg = calcAverageSearch(pattern, COINTOSSES)
    #print(f'average search length for {pattern} pattern: {avg:.3f}')

    patterns = ( f'{a:09b}'[-pattern_length:] for a in range(2**pattern_length) )
    res = { pattern:calcAverageSearch(pattern, COINTOSSES) for pattern in patterns }
    for ptrn, avg in res.items():
        print(f'average search length for \'{ptrn}\' pattern is {avg:.3f} coin tosses')


def calcAverageSearch(pattern, cointosses=10_000):
    tosses = ''.join(str(random.randint(0, 1)) for _ in range(cointosses))
    sequence_matches = re.split(pattern, tosses)[:-1]  # remove last element (not a match)
    average_search = mean(len(seq) for seq in sequence_matches) + len(pattern)
    return average_search


if __name__ == '__main__':
    main()