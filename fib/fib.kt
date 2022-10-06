fun main() {
    println(fib(10))
}

fun fib(n: Int): Int {
  if (n < 2) {
    return n
  } else {
    return fib(n-1) + fib(n-2)
  }
}