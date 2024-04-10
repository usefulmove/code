#lang racket


;; core "data types"
;;   board - [int] (length 81)
;;   cell - int (0-80)
;;   row - int (0-8)
;;   col - int (0-8)
;;   box - int (0-8)
;;   digit - int (1-9)


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


(define valid-digits
  (range 1 (add1 9)))


;; display-board :: board -> null (impure)
;;               :: [int] -> null
(define (display-board board)
  (newline)
  (let ((display-row (lambda (row)
                       (for-each
                        (lambda (digit)
                          (display (format " ~a " digit)))
                        (get-row-digits board row))
                       (newline))))
    (for-each display-row (range 9)))
  (newline))


;; get-cell-digit :: board -> cell -> digit
;;                :: [int] -> int -> -> int
(define get-cell-digit list-ref) ; list-ref ( point-free )


;; set-cell-digit :: board -> cell -> digit -> board
;;                :: [int] -> int -> -> int -> [int]
(define (set-cell-digit board cell digit)
  (append (take board cell)
          (list digit)
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


;; zip :: [T] -> [U] -> ... -> [T U ...]
(define (zip lst . lsts)
  (apply map list lst lsts))


;; get-row-digits :: board -> row -> [digits]
;;                :: [int] -> int -> [int]
(define (get-row-digits board row)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= row (get-row index))))
                         (zip (range (length board))
                              board))))
    (map
     cadr
     matching-pairs)))


;; get-col-digits :: board -> col -> [digits]
;;                :: [int] -> int -> [int]
(define (get-col-digits board col)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= col (get-col index))))
                         (zip (range (length board))
                              board))))
    (map
     cadr
     matching-pairs)))


;; get-box-digits :: board -> box -> [digits]
;;                :: [int] -> int -> [int]
(define (get-box-digits board box)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= box (get-box index))))
                         (zip (range (length board))
                              board))))
    (map
     cadr
     matching-pairs)))


; find-empty-cell :: board -> cell
;                 :: board -> int
(define (find-empty-cell board)
  (index-of board 0))


; get-non-candidates :: board -> cell -> [digits]
;                    :: [int] -> int -> [int]
(define (get-non-candidates board cell)
  (remove-duplicates
   (append (get-row-digits board (get-row cell))
           (get-col-digits board (get-col cell))
           (get-box-digits board (get-box cell)))))


; possible? :: board -> cell -> digit -> boolean
;           :: [int] -> int -> int -> boolean
(define (possible? board cell digit)
  (not (member digit (get-non-candidates board cell))))


;; solved? :: board -> boolean
;;         :: [int] -> boolean
(define (solved? board)
  (not (or (null? board) ; board is not null
           (member 0 board)))) ; board contains no zeros


;; backtracking solver
;; solve :: board -> board
;;       :: [int] -> [int] (empty list if no solution found)
(define (solve board)
  (call-with-current-continuation
   (lambda (return)
     (let ((empty-cell (find-empty-cell board)))
       (if (false? empty-cell)
           board ; return solution.
           (begin
             (for ((candidate valid-digits))
               (when (possible? board empty-cell candidate)
                 (let ((possible-solution
                        (solve(set-cell-digit board empty-cell candidate))))
                   (when (solved? possible-solution)
                     (return possible-solution))))) ; return solution.
             '())))))) ; all candidates exhausted. no solution found.



(display-board (solve original-board))
