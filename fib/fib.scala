@main def main(args: String*) =
  printf("%d\n", fib2(args(0).toInt))

  /* recursive solution */
  def fib(n: Int): Int =
    n match
      case 0 | 1 => n
      case _ => fib(n-1) + fib(n-2)

  /* tail recursive solution */
  def fib2(n: Int, a: Int = 0, b: Int = 1): Int =
    n match
      case 0 => return a
      case 1 => return b
      case _ => fib2(n-1, b, a+b)