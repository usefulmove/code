#lang racket


(require algorithms)


;; core "data types":
;;   board - [int] (length 81)
;;   cell - int (0-80)
;;   row - int (0-8)
;;   col - int (0-8)
;;   box - int (0-8)
;;   value - int (1-9)



(define original-board
  (list 5 3 0 0 7 0 0 0 0
        6 0 0 1 9 5 0 0 0
        0 9 8 0 0 0 0 6 0
        8 0 0 0 6 0 0 0 3
        4 0 0 8 0 3 0 0 1
        7 0 0 0 2 0 0 0 6
        0 6 0 0 0 0 2 8 0
        0 0 0 4 1 9 0 0 5
        0 0 0 0 8 0 0 7 9 ))


(define (display-board board)
  (displayln (take board 9))
  (displayln (drop (take board 18) 9))
  (displayln (drop (take board 27) 18))
  (displayln (drop (take board 36) 27))
  (displayln (drop (take board 45) 36))
  (displayln (drop (take board 54) 45))
  (displayln (drop (take board 63) 54))
  (displayln (drop (take board 72) 63))
  (displayln (drop (take board 81) 72)))


;; get-cell-value :: board -> cell -> value
;;                :: [int] -> int -> -> int
(define get-cell-value list-ref) ; list-ref ( point-free )


;; set-cell-value :: board -> cell -> value -> board
;;                :: [int] -> int -> -> int -> [int]
(define (set-cell-value board cell value)
  (append (take board cell)
          (list value)
          (drop board (add1 cell))))


; get-row :: cell -> row
;         :: int -> int
(define (get-row cell)
  (floor (/ cell 9)))


; get-col :: cell -> col
;         :: int -> int
(define (get-col cell)
  (modulo cell 9))


; get-box :: cell -> box
;         :: int -> int
(define (get-box cell)
  (let ((row (get-row cell))
        (col (get-col cell)))
    (+ (* (floor (/ row 3)) 3)
       (floor (/ col 3)))))


;; get-row-values :: board -> row -> [values]
;;                :: [int] -> int -> [int]
(define (get-row-values board row)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair))
                                 (value (cdr pair)))
                             (= row (get-row index))))
                         (zip (range (length board))
                              board))))
    (map
     cadr
     matching-pairs)))


;; get-col-values :: board -> col -> [values]
;;                :: [int] -> int -> [int]
(define (get-col-values board col)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair))
                                 (value (cdr pair)))
                             (= col (get-col index))))
                         (zip (range (length board))
                              board))))
    (map
     cadr
     matching-pairs)))


;; get-box-values :: board -> box -> [values]
;;                :: [int] -> int -> [int]
(define (get-box-values board box)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair))
                                 (value (cdr pair)))
                             (= box (get-box index))))
                         (zip (range (length board))
                              board))))
    (map
     cadr
     matching-pairs)))


; find-empty-cell :: board -> cell
;               :: board -> int
(define (find-empty-cell board)
  (index-of board 0))


; get-non-candidates :: board -> cell -> [values]
;                    :: [int] -> int -> [int]
(define (get-non-candidates board cell)
  (remove-duplicates
   (append (get-row-values board (get-row cell))
           (get-col-values board (get-col cell))
           (get-box-values board (get-box cell)))))


; possible? :: board -> cell -> value -> boolean
;           :: [int] -> int -> int -> boolean
(define (possible? board cell value)
  (not (member value (get-non-candidates board cell))))


;; solved? :: board -> boolean
;;         :: [int] -> boolean
(define (solved? board)
  (not (member 0 board)))


;; solve :: board -> board
;; solve :: [int] -> [int] (empty list if fails to solve the board)
(define (solve board)
  (call/cc
   (lambda (return)
     (let ((empty-cell (find-empty-cell board)))
       (if (false? empty-cell)
           board ; return solution.
           (begin
             (for ((candidate (range 1 (add1 9))))
               (if (possible? board empty-cell candidate)
                   (let ((possible-solution (solve(set-cell-value board empty-cell candidate))))
                     (if (solved? possible-solution)
                         (return possible-solution) ; return solution.
                         (void)))
                   (void)))
             null)))))) ; all candidates exhausted. no solution found.



(solve original-board)
