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


; solve :: [[int]] -> (int . int) -> [int] -> [[int]] (todo)
(define solve
  (lambda (board pos attempted)
    (let ((rank (car pos))
          (file (cdr pos)))
      (cond ((= rank -1) board) ; solution found. return board.
            ((equal? (sort attempted <) (range 1 10)) empty) ; no solution. failed all attempts.
            ((not (zero? (get-value board pos))) (solve board (get-next-pos pos) empty)) ; solve next position
            (else (let (result (solve (set-value board pos (todo)) (get-next-pos pos) empty) ; zero at current position
                    (if (empty? result)
                      empty ; no solution.
                      (solve board pos (cons (todo)
                                             attempted)))))))))) ; solve with the next attempt

      ;(if (= -1 (car first-zero-pos))
      ;  board ; no zeros. solution found. return board.
      ;  (let ((valid-values (get-valid-values board first-zero-pos))
      ;        (valid-values-minus-attempts (set->list (todo))))
      ;    (if (empty? valid-values-minus-attempts)
      ;      empty ; no valid solution exists
      ;      (let ((result (solve
      ;                      (set-value board first-zero-pos (car valid-values-minus-attempts))
      ;                      empty)))
      ;        (if (not (empty? result))
      ;          result ; valid solution
      ;          (solve
      ;            (set-value board first-zero-pos (cadr valid-values-minus-attempts))
      ;            empty)))))))))


; get-next-pos :: (int . int) -> (int . int)
(define get-next-pos
  (lambda (pos)
    (todo)))


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
        empty))))


; get-first-zero-pos :: [[int]] -> (int . int)
(define get-first-zero-pos
  (lambda (board)
    (todo)))


(solve board '(0 . 0) empty)