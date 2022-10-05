#!/usr/bin/python3

import sys

def fib(n):
  if n < 2:
    return n
  else:
    return fib(n-1) + fib(n-2)

if len(sys.argv) > 1:
  print(str(fib(int(sys.argv[1]))))