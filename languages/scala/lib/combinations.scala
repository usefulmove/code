@main
def main(args: String*): Unit =
  val nums = List[Int](3, 2, 1, 5, 4)

  /* generate all possibile combinations of elements */

  val combinations = (1 until scala.math.pow(2, nums.length).toInt)
  // create n-bit bit field with all possible bitmasks
  .map {_.toBinaryString}
  .map {mask =>
    mask
    .reverse
    .zip {nums} // (multiplier, value)
    .foldLeft(List[Int]()) {(acc, p) =>
      val (c, a) = p
      c match
        case '0' => acc
        case _ => a :: acc }
  }

  combinations foreach println