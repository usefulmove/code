@main
def main(args: String*): Unit =
  val arg = args.length match
    case 0 => "9781984801821"
    case _ => args(0)

  println(checksum(arg))

def checksum(pattern: String): Int =
  pattern
  .zip(
    (1 to pattern.length)
    .map(n => if (n & 1) == 0 then 3 else 1)
   )
  .map(_.toString.toInt * _)
  .sum % 10