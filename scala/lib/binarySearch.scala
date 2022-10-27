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
          var (left, mid, right) = (0, 0, arr.length - 1)

          while (left <= right) {
              mid = ((left + right) >> 1) 
              
              if (arr(mid) == target) return Right(mid)

              target > arr(mid) match {
                  case true => left = mid + 1
                  case _ => right = mid - 1
              }
          }

          return Left(mid)
      }
      
}
