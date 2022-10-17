@main
def main(args: String*): Unit =
  //println(args(0))
  println(checksum(args(0)))

def checksum(s: String): Int =
  s
  .map(_.toInt - '0'.toInt)
  .zip(
    (1 to s.length)
    .map(a => if a % 2 == 0 then 3 else 1)
   )
  .map((a, b) => a * b)
  .sum % 10
