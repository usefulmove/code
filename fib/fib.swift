func fib(n: Int) -> Int {
  if (n < 2) {
    return n
  } else {
    return fib(n: n-1) + fib(n: n-2)
  }
}

print(fib(n: 38))
