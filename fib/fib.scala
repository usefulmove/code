object Fib {
    def main(args: Array[String]) = {
        printf("%d\n", fib(10))
    }

    def fib(n: Int): Int = {
        if (n < 2) { return n }
        else { return fib(n - 1) + fib(n - 2) }
    }
}
