sub fib(Int $n) {
    if $n < 2 {
      return $n
    }  else {
      return fib($n-1) + fib($n-2)
    }
}

printf fib(10);