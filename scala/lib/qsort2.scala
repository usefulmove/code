object QSort:
    @main def main(args: String*): Unit =
        //val list = List(3,1,2,5,4)
        val list = args map {_.toInt}
        qsort(list) foreach {println}

    def qsort(seq: Seq[Int]): Seq[Int] =
        var os = seq
        os.length match
            case 0 | 1 => os
            case _ =>
                var piv = 0
                for a <- 1 until os.length do
                    if os(a) < os(piv) then
                        os = os(a) +: (os.slice(0, a) ++ os.slice(a + 1, os.length))
                        piv += 1
                qsort(os.slice(0, piv)) ++ Seq(os(piv)) ++ qsort(os.slice(piv + 1, os.length))