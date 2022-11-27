qsort :: [Int] -> [Int]
qsort [] = []
qsort [a] = [a]
qsort (a:as) = qsort small ++ [a] ++ qsort large
    where small = [x | x <- as, x <= a]
          large = [x | x <- as, x > a]