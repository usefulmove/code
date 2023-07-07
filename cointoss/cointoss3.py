#!/bin/python3

import random
import re
from statistics import mean
from typing import Dict, Generator


def main() -> None:
    COINTOSSES: int = 8_000_000
    print(f'calculating average search lengths for {COINTOSSES:,} coin tosses')

    pattern_length: int = 5  # must be less than 10

    # pattern = r'011'
    # avg = calcAverageSearch(pattern, COINTOSSES)
    # print(f'average search length for {pattern} pattern: {avg:.3f}')

    # pattern = r'101'
    # avg = calcAverageSearch(pattern, COINTOSSES)
    # print(f'average search length for {pattern} pattern: {avg:.3f}')

    patterns: Generator = (
        f'{a:09b}'[-pattern_length:] for a in range(2**pattern_length)
    )
    res: Dict[str, float] = {
        pattern: calcAverageSearch(pattern, COINTOSSES) for pattern in patterns
    }
    for ptn, avg in res.items():
        print(
            f'average search length for \'{ptn}\' pattern is {avg:.3f} tosses'
        )


def calcAverageSearch(pattern: str, cointosses: int = 10_000) -> float:
    tosses = ''.join(str(random.randint(0, 1)) for _ in range(cointosses))
    sequence_matches = re.split(pattern, tosses)[:-1]  # remove last element
    average_search = mean(len(seq) for seq in sequence_matches) + len(pattern)
    return average_search


if __name__ == '__main__':
    main()
