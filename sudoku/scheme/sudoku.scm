#lang racket


; sudoku board
(define board
  (list
   (list 5 3 0 0 7 0 0 0 0)
   (list 6 0 0 1 9 5 0 0 0)
   (list 0 9 8 0 0 0 0 6 0)
   (list 8 0 0 0 6 0 0 0 3)
   (list 4 0 0 8 0 3 0 0 1)
   (list 7 0 0 0 2 0 0 0 6)
   (list 0 6 0 0 0 0 2 8 0)
   (list 0 0 0 4 1 9 0 0 5)
   (list 0 0 0 0 8 0 0 7 9)))


; get-value :: int -> int -> int
; get-value 5 7 => 5 (zero-based)
(define get-value (lambda (board rank file)
                    (list-ref
                      (list-ref
                      	board
                      	(- 8 file))
                      rank)))


; solve :: [[int]] -> [[int]]
(define solve
  (lambda (board)
    (let ((first-zero-pos (get-first-zero-pos board)))
      (if (= -1 (car first-zero-pos))
        board ; solution found. return board.
        (let ((possibles (get-possibles board first-zero-pos)))
          (if (empty? possibles)
            empty ; no solution exists
            (solve
              (set-possible first-zero-pos (car possibles))
              (car possibles))))))))


; get-possibles :: [[int]] -> (int . int) -> [int]
(define get-possibles
  (lambda (board pos)
    (todo)))


; set-possible :: (int . int) -> int -> [[int]]
(define set-possible
  (todo))


; get-first-zero-pos :: [[int]] -> (int . int)
(define get-first-zero-pos
  (lambda (board)
    (todo)))