#!/usr/bin/python3

import sys

def fib(n):
  if n < 2:
    return n
  else:
    return fib(n-1) + fib(n-2)

def fib2(n, a = 0, b = 1):
  if n == 0:
    return a
  if n == 1:
    return b
  return fib2(n - 1, b, a + b)


if len(sys.argv) > 1:
  print(str(fib(int(sys.argv[1]))))