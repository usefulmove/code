import scala.annotation.tailrec

object BinarySearch {
      @main
      def main(target: String, args: String*) = {
          val out = binSearch(args.map(s => s.toInt).toArray, target.toInt) match {
              case Right(ind) => ind
              case _ => -1
          }
          println(out)
      }


      def binSearch(arr: Array[Int], target: Int): Either[Int, Int] = {
        
          @tailrec
          def helper(left: Int, right: Int): Either[Int, Int] = {
              val mid = ((left + right) >> 1) 
              return mid match {
                  case mid if (arr(mid) == target) => Right(mid)
                  case mid if (left <= right) => {
                      target > arr(mid) match
                        case true => helper(mid + 1, right)
                        case _ => helper(left, mid - 1)
                  }
                  case _ => Left(mid)
              }
          }

          helper(0, arr.length - 1)
      }

}
