(load-file "~/repos/cora/src/cora.elc") ; t


#|   Think of the board as a one-dimensional tensor, rather than as a grid.  |#

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


                                        ; access the board by row and column using the modulus function


(setf get-square-value 'list-ref) ; list-ref ( point-free )

(get-square-value board 80) ; 9


; dynamic programming

;; evaluate-board :: board -> position -> board
;;                :: [int] -> int -> [int]
(defun evaluate-board (input-board pos)
  "Evaluate the INPUT BOARD at the specified position (POS). Search for a value
on the range 0 to 9 that satisfies the row, column, and cell constraints for that
position. When the first candidate is found, insert it and 'pass it on'.
Evaluate (recursively) the new board at the next position. If that succeeds, we
have a solved board. If not, move on to the next candidate."
  (let ((value (get-square-value input-board pos)))  ; todo - defun get-square-value (board pos)
    (if (not (zero? value))
        (evaluate-board
         input-board
         (inc pos))
      (foldl
       (lambda (in-board candidate-value)
         (if (possible-value? in-board candidate-value)
             (evaluate-board
              (set-square-value in-board pos candidate-value) ; todo - defun set-square-value (board pos value)
              (inc pos))
           in-board))
       input-board
       (range 1 (inc 9)))))) ; iterate over value candidates 1 - 9.


(foldl
 'evaluate-board
 board
 (range (length board)))
