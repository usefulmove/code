function fib(n: number): number {
  if (n < 2) {
    return n
  } else {
    return ( fib(n-1) + fib(n-2) )
  }
}

console.log(fib(38))
