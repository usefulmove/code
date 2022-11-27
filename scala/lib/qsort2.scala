object QSort:
    @main def main(args: String*): Unit =
        val list = args map {_.toDouble}
        qsort(list) foreach {println}

    def qsort[T](seq: Seq[T])(implicit order: Ordering[T]): Seq[T] =
        seq match
            case Seq() | Seq(_) => seq
            case piv +: rest =>
                val (small, large) = rest partition {order.lt(_, piv)}
                qsort(small) ++ Seq(piv) ++ qsort(large)