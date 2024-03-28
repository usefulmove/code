(load-file "~/repos/cora/src/cora.el")


(setq original-board
  (list 5 3 0 0 7 0 0 0 0
        6 0 0 1 9 5 0 0 0
        0 9 8 0 0 0 0 6 0
        8 0 0 0 6 0 0 0 3
        4 0 0 8 0 3 0 0 1
        7 0 0 0 2 0 0 0 6
        0 6 0 0 0 0 2 8 0
        0 0 0 4 1 9 0 0 5
        0 0 0 0 8 0 0 7 9 ))


;; get-cell-value :: board -> cell -> board
;;                :: [int] -> int -> -> [int]
(setf get-cell-value 'list-ref)  ; list-ref ( point-free )


;; set-cell-value :: board -> cell -> value -> board
;;                :: [int] -> int -> -> int -> [int]
(defun set-cell-value (board cell value)
  "Insert VALUE into the BOARD provided at the cell (CELL) specified and
return a new board."
  (append (take cell board)
          (list value)
          (drop (inc cell) board)))


; get-row :: cell -> row (0-8)
;         :: int -> int
(defun get-row (cell)
  (floor cell 9))


; get-col :: cell -> col (0-8)
;         :: int -> int
(defun get-col (cell)
  (mod cell 9))


; get-box :: cell -> box (0-8)
;         :: int -> int
(defun get-box (cell)
  (let ((row (get-row cell))
        (col (get-col cell)))
    (+ (* (floor row 3) 3)
       (floor col 3))))


;; get-row-values :: board -> row -> [values]
;;                :: [int] -> int -> [int]
(defun get-row-values (board row)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair))
                                 (value (cdr pair)))
                             (= row (get-row index))))
                         (zip (range (length board))
                              board))))
    (map
     'cdr
     matching-pairs)))


;; get-col-values :: board -> col -> [values]
;;                :: [int] -> int -> [int]
(defun get-col-values (board col)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair))
                                 (value (cdr pair)))
                             (= col (get-col index))))
                         (zip (range (length board))
                              board))))
    (map
     'cdr
     matching-pairs)))


;; get-box-values :: board -> box -> [values]
;;                :: [int] -> int -> [int]
(defun get-box-values (board box)
  (let ((matching-pairs (filter
                         (lambda (pair)
                           (let ((index (car pair))
                                 (value (cdr pair)))
                             (= box (get-box index))))
                         (zip (range (length board))
                              board))))
    (map
     'cdr
     matching-pairs)))


; get-non-candidates :: board -> cell -> [values]
;                    :: [int] -> int -> [int]
(defun get-non-candidates (board cell)
  (remove-duplicates
   (append (get-row-values board (get-row cell))
           (get-col-values board (get-col cell))
           (get-box-values board (get-box cell)))))


; possible? :: board -> cell -> value -> boolean
;           :: [int] -> int -> int -> boolean
(defun possible? (board cell value)
  (not (member value (get-non-candidates board cell))))


;; solved? :: board -> boolean
;;         :: [int] -> boolean
(defun solved? (board)
  (not (member 0 board)))


;; todo - rethink this strategy to remove fold implementation as is and allow early exit when successful solution is found
;;; evaluate-board :: board -> cell -> board
;;;                :: [int] -> int -> [int]
;(defun evaluate-board (board cell)
;  "Evaluate the INPUT BOARD at the specified cell (CELL). Search for a value
;on the range 0 to 9 that satisfies the row, column, and box constraints for that
;cell. When the first candidate is found, insert it and 'pass it on'.
;Evaluate (recursively) the new board at the next cell. If that succeeds, we
;have a solved board. If not, move on to the next candidate."
;  (if (or (solved? board)
;          (= cell (length board)))
;      board
;    (let ((cell-value (call get-cell-value board cell)))
;      (if (not (zero? cell-value))
;          (evaluate-board
;           board
;           (inc cell))
;        (foldl
;         (lambda (in-board candidate-value)
;           (if (possible? in-board cell candidate-value)
;               (evaluate-board
;                (set-cell-value in-board cell candidate-value)
;                (inc cell))
;             in-board))
;         board
;         (range 1 (inc 9)))) ; iterate over value candidates (1-9).
;      '()))) ; fail - return empty list.
;
;
;; todo - rethink this strategy to remove fold implementation as is and allow early exit when successful solution is found
;(defun solve (board)
;  (foldl
;   'evaluate-board
;   board
;   (range (length original-board))))
;
;(solve original-board)
