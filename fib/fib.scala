object Fib {
    def main(args: Array[String]) = {
        printf("%d\n", fib(args(0).toInt))
    }

    def fib(n: Int): Int = {
        n match
            case 0 => 0
            case 1 | 2 => 1
            case _ => fib(n - 1) + fib(n - 2)
    }
}
