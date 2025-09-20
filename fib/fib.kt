fun main() {
  println(fib(10))
}

fun fib(n: Int): Int {
  when (n) {
    0, 1 -> return n
    else -> return fib(n-1) + fib(n-2)
  }
}