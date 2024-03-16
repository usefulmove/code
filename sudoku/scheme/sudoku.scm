#lang racket


; sudoku board :: [[int]]
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


; get-value :: [[int]] -> (int . int) -> int
(define get-value (lambda (board pos)
                    (let ((rank (car pos))
                          (file (cdr pos)))
                      (list-ref
                        (list-ref
                        	board
                        	(- 8 rank))
                        file))))


; set-value :: [[int]] -> (int . int) -> int -> [[int]]
(define set-value
  (lambda (board pos value)
    (todo)))


; solve :: [[int]] -> [int] -> [[int]] (todo)
(define solve
  (lambda (board lst-to-attempt-orig)
    (let ((first-zero-pos (get-first-zero-pos board)))
      (if (= -1 (car first-zero-pos))
        board ; solution found. return board.
        (let ((valid-values (get-valid-values board first-zero-pos))
              (lst-to-attempt (todo)))
          (if (empty? valid-values)
            '() ; no valid solution exists
            (solve ; (todo)
              (set-value board first-zero-pos (car lst-to-attempt))
              (cdr lst-to-attempt-orig))))))))


; get-valid-values :: [[int]] -> (int . int) -> [int]
(define get-valid-values
  (lambda (board pos)
    (let* ((rank (car pos))
           (file (cdr pos))
           (cell (todo))
           (get-valid-values-rank (lambda (board file)
                                    (todo)))
           (get-valid-values-file (lambda (board rank)
                                    (todo)))
           (get-valid-values-cell (lambda (board cell)
                                    (todo))))
      (if (zero? (get-value board pos))
        (set->list (set-intersect (get-valid-values-rank board rank)
                                  (get-valid-values-file board file)
                                  (get-valid-values-cell board cell)))
        '()))))


; get-first-zero-pos :: [[int]] -> (int . int)
(define get-first-zero-pos
  (lambda (board)
    (todo)))


(solve board (range 1 10))