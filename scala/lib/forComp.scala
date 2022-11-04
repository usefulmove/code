object Comp:
  @main
  def main(args: String*) =
    val squares =
      for
        c <- 'a' to 'h'
        r <- 1 to 8
      yield c.toString + r.toString

    squares foreach println
