object Solution {
    def sortArray(nums: Array[Int]): Array[Int] = {
        qsort(nums)
    }
    
    def qsort(vec: Array[Int]): Array[Int] = {
        val cop = vec
    
        cop.length match {
            case 0 | 1 => vec
            case _ => {
                // use last element as pivot
                val pindex = cop.length/2
                swap(cop, pindex, cop.length-1)
                val pivot = cop(cop.length-1)
    
                var ins = 0
                for (icomp <- 0 to cop.length-2) {
                    if (cop(icomp) < cop(cop.length-1)) {
                        swap(cop, icomp, ins)
                        ins += 1
                    }
                }
                swap(cop, ins, cop.length-1)
    
                val left: Array[Int] = {
                    ins match {
                        case 0 => Array[Int]()
                        case _ => cop.slice(0, ins)
                    }
                }
                val right = cop.slice(ins+1, cop.length)
    
                qsort(left) ++ (pivot +: qsort(right))
            }
        }
    }
    
    def swap(vec: Array[Int], i: Int, j: Int) {
        if (vec.length >= i && vec.length >= j) {
            val o = vec(i)
            vec(i) = vec(j)
            vec(j) = o
        }
    }
}
