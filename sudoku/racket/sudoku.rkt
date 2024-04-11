#lang racket


;; core "data types"
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


(define valid-values
  (range 1 (add1 9)))


;; display-board :: board -> null (impure)
;;               :: [int] -> null
(define (display-board board)
  (newline)
  (let ((display-row (lambda (row)
                       (for-each
                        (lambda (value)
                          (display (format " ~a " value)))
                        (get-row-values board row))
                       (newline))))
    (for-each display-row (range 9)))
  (newline))


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


;; zip :: [T] -> [U] -> ... -> [T U ...]
(define (zip lst . lsts)
  (apply map list lst lsts))


;; get-row-values :: board -> row -> [values]
;;                :: [int] -> int -> [int]
(define (get-row-values board row)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= row (get-row index))))
                         (zip (range (length board)) board))))
    (map cadr matching-pairs)))


;; get-col-values :: board -> col -> [values]
;;                :: [int] -> int -> [int]
(define (get-col-values board col)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= col (get-col index))))
                         (zip (range (length board)) board))))
    (map cadr matching-pairs)))


;; get-box-values :: board -> box -> [values]
;;                :: [int] -> int -> [int]
(define (get-box-values board box)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair)))
                             (= box (get-box index))))
                         (zip (range (length board)) board))))
    (map cadr matching-pairs)))


; get-first-empty-cell :: board -> cell
;                 :: [int] -> int
; note: get-first-empty-cell returns #f no empty cells are found
(define (get-first-empty-cell board)
  (index-of board 0))


; get-non-candidates :: board -> cell -> [value]
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


;; board-solved? :: board -> boolean
;;         :: [int] -> boolean
(define (board-solved? board)
  (and (not (null? board)) ; board is not null
       (not (get-first-empty-cell board)))) ; board contains no empty cells


;; backtracking solver
;; solve-board :: board -> board
;;                  :: [int] -> [int] (empty list if no solution found)
(define (solve-board board)
  (call-with-current-continuation
   (lambda (return)
     (let ((empty-cell (get-first-empty-cell board)))
       (if (not empty-cell)
           board ; return solved board.
           (begin
             (for ((candidate valid-values))
               (when (possible? board empty-cell candidate)
                 (let ((could-be-solution (solve-board
                                           (set-cell-value
                                            board
                                            empty-cell
                                            candidate))))
                   (when (board-solved? could-be-solution)
                     (return could-be-solution))))) ; return solved board.
             '())))))) ; all candidates exhausted. no solution found.



(display-board (solve-board original-board))
