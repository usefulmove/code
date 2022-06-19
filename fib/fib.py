#!/usr/bin/python3

import sys

def fib(n):
  if n <= 0:
    return 0
  elif n < 3:
    return 1
  else:
    return fib(n-1) + fib(n-2)

if len(sys.argv) > 1:
  print(str(fib(int(sys.argv[1]))))
else:
  print("( error: nope )")
