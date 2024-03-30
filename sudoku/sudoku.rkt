#lang racket


;; core "data types":
;;   board - [int] (length 81)
;;   cell - int (0-80)
;;   value - int (0-9)


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




