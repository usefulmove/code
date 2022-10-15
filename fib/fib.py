#!/usr/bin/python3

import sys

# recursive solution
def fib(n):
  match n:
    case 0 | 1:
      return n
    case _:
      return fib(n-1) + fib(n-2)

# tail recursive solution
def fib2(n, a = 0, b = 1):
  match n:
    case 0: return a
    case 1: return b
    case _: return fib2(n - 1, b, a + b)


if len(sys.argv) > 1:
  print(str(fib2(int(sys.argv[1]))))