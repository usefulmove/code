#!node

function fib(n) {
  if (n < 2) {
    return n
  } else {
    return ( fib(n-1) + fib(n-2) )
  }
}

function fib2(n, a = 0, b = 1) {
  if (n == 0) {
    return a
  } else if (n == 1) {
    return b
  } else {
    return (fib2(n-1, b, a+b))
  }
}


console.log(fib2(38));