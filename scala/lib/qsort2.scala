object QSort:
    @main def main(args: String*): Unit =
        //val list = List(3,1,2,5,4)
        //val list = List(3.1,1,2.2,5.0,4)
        val list = args map {_.toDouble}
        qsort(list) foreach {println}

    def qsort[T](sequence: Seq[T])(implicit order: Ordering[T]): Seq[T] =
        var seq = sequence
        seq.length match
            case 0 | 1 => seq
            case _ =>
                var piv = 0 // pivot
                for a <- 1 until seq.length do
                    if order.lt(seq(a), seq(piv)) then
                        seq = seq(a) +: (seq.slice(0, a) ++ seq.slice(a + 1, seq.length))
                        piv += 1
                qsort(seq.slice(0, piv)) ++ Seq(seq(piv)) ++ qsort(seq.slice(piv + 1, seq.length))