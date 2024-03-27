(load-file "~/repos/cora/src/cora.elc") ; t


(setq board
  (list 5 3 0 0 7 0 0 0 0
        6 0 0 1 9 5 0 0 0
        0 9 8 0 0 0 0 6 0
        8 0 0 0 6 0 0 0 3
        4 0 0 8 0 3 0 0 1
        7 0 0 0 2 0 0 0 6
        0 6 0 0 0 0 2 8 0
        0 0 0 4 1 9 0 0 5
        0 0 0 0 8 0 0 7 9 ))


; access the board by row and column using the modulus function in helper functions - only necessary for block determination (possibly true)
; language: board, cell, block




; dynamic programming

;; get-cell-value :: board -> pos -> board
;;                  :: [int] -> int -> -> [int]
(setf get-cell-value 'list-ref) ; list-ref ( point-free )
;(get-cell-value board 80) ; 9


;; set-cell-value :: board -> pos -> value -> board
;;                  :: [int] -> int -> -> int -> [int]
(defun set-cell-value (board pos value)
  "Insert VALUE into the BOARD provided at the pos (POS) specified and
return a new board."
  todo) 


;; possible? :: board -> position -> value -> boolean
;;           :: [int] -> int -> int -> boolean
(defun possible? (board pos value)
  (and (possible-row? board pos value) ; todo
       (possible-col? board pos value) ; todo
       (possible-block? board pos value))) ; todo
; todo - convert to make use of set data structure?


;; evaluate-board :: board -> pos -> board
;;                :: [int] -> int -> [int]
(defun evaluate-board (input-board pos)
  "Evaluate the INPUT BOARD at the specified pos (POS). Search for a value
on the range 0 to 9 that satisfies the row, column, and block constraints for that
pos. When the first candidate is found, insert it and 'pass it on'.
Evaluate (recursively) the new board at the next pos. If that succeeds, we
have a solved board. If not, move on to the next candidate."
  (let ((value (get-cell-value input-board pos)))
    (if (not (zero? value))
        (evaluate-board
         input-board
         (inc pos))
      (foldl
       (lambda (in-board candidate-value)
         (if (possible? in-board pos candidate-value)
             (evaluate-board
              (set-cell-value in-board pos candidate-value)
              (inc pos))
           in-board))
       input-board
       (range 1 (inc 9)))))) ; iterate over value candidates 1 - 9.
; todo - missing null return when no answer found for any candidate.


(foldl
 'evaluate-board
 board
 (range (length board)))
