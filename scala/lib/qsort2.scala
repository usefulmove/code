object QSort:
    @main def main(args: String*): Unit =
        val list = args map {_.toDouble}
        qsort(list) foreach {println}

    def qsort[T](seq: Seq[T])(implicit order: Ordering[T]): Seq[T] =
        seq.length match
            case 0 | 1 => seq
            case _ =>
                val piv :: rest = seq : @unchecked
                val (small, large) = rest partition {order.lt(_, piv)}
                qsort(small) ++ Seq(piv) ++ qsort(large)