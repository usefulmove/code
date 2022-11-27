qsort :: [Int] -> [Int]
qsort [] = []
qsort [o] = [o]
qsort (o:os) = qsort small ++ [o] ++ qsort large
    where small = [x | x <- os, x <= o]
          large = [x | x <- os, x > o]