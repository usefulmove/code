object BinarySearch:
      @main
      def main(target: String, args: String*) =
          val out =
              binSearch(args.map(_.toInt).toArray, target.toInt) match
                  case Right(ind) => ind
                  case _ => -1
          println {out}

      def binSearch(arr: Array[Int], target: Int): Either[Int, Int] =

          @scala.annotation.tailrec
          def recurse(left: Int, right: Int): Either[Int, Int] =
              val mid = (left + right) >> 1
              return mid match
                  case mid if left > right => Left(mid + 1)
                  case mid if arr(mid) == target => Right(mid)
                  case _ => target > arr(mid) match
                      case true => recurse(mid + 1, right)
                      case _ => recurse(left, mid - 1)

          recurse(0, arr.length - 1)