import scala.annotation.tailrec

object Fib:
  @main def main(args: String*) =
    println(s"  ${fib2(args(0).toInt)}")

  /* recursive solution */
  def fib(n: Long): Long =
    n match
      case 0 | 1 => n
      case _ => fib(n-1) + fib(n-2)

  /* tail recursive solution */
  @tailrec
  def fib2(n: Long, a: Long = 0, b: Long = 1): Long =
    n match
      case 0 => a
      case 1 => b
      case _ => fib2(n-1, b, a+b)