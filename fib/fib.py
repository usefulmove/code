#!/usr/bin/python3

from functools import cache
import sys


# recursive solution
@cache
def fib(n: int) -> int:
    match n:
        case 0 | 1: return n
        case _: return fib(n-1) + fib(n-2)


# tail recursive solution
@cache
def fib2(n: int, a: int = 0, b: int = 1) -> int:
    match n:
        case 0: return a
        case 1: return b
        case _: return fib2(n - 1, b, a + b)


def main():
    if len(sys.argv) > 1:
        print(fib2(int(sys.argv[1])))


if __name__ == '__main__':
    main()
