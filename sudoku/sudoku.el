(load-file "~/repos/cora/src/cora.el")


(setq original-board '(( 0 0 1 8 0 0 0 0 0 )
                       ( 6 7 0 2 0 3 0 1 0 )
                       ( 2 0 5 7 0 0 6 0 3 )
                       ( 3 5 0 6 2 0 8 0 0 )
                       ( 7 6 2 0 5 8 3 0 0 )
                       ( 0 0 0 0 0 4 0 5 0 )
                       ( 0 9 0 5 8 6 0 0 0 )
                       ( 5 0 0 9 0 0 7 0 0 )
                       ( 0 2 6 4 3 0 5 9 0 )))


(setq valid-digits (range 1 (inc 9)))


;; get-cell :: integer -> integer -> integer
(defun get-cell (row col)
  "Return the cell index given the ROW and COL indicies."
  (let ((a (/ row 3))
        (b (/ col 3)))
    (cond ((= 0 a) (cond ((= 0 b) 0)
                         ((= 1 b) 1)
                         ((= 2 b) 2)))
          ((= 1 a) (cond ((= 0 b) 3)
                         ((= 1 b) 4)
                         ((= 2 b) 5)))
          ((= 2 a) (cond ((= 0 b) 6)
                         ((= 1 b) 7)
                         ((= 2 b) 8))))))


;; read-col-digits :: [[integer]] -> [integers]
(defun read-col-digits (board col)
  "Return the set of digits assigned to the column (COL) on the BOARD."
  (let ((digits (foldl
                  (lambda (outer-acc row)
                    (foldl
                      (lambda (inner-acc pair)
                        (if (= col (car pair)) ; if column
                            (cons (cdr pair) inner-acc) ; add to accumulator
                          inner-acc))
                      outer-acc
                      (enumerate row)))
                  '()
                  board)))
    (remove-duplicates digits)))


;; read-row-digits :: [[integer]] -> [integers]
(defun read-row-digits (board row)
  "Return the set of digits assigned to the ROW on the BOARD."
  (let ((digits (list-ref board row)))
    (remove-duplicates digits)))


;; read-cell-digits :: [[integer]] -> [integers]
(defun read-cell-digits (board cell)
  "Return the set of digits assigned to the CELL on the BOARD."
  (let ((digits (foldl
                  (lambda (outer-acc enum-row)
                    (let ((row (car enum-row))
                          (row-digits (cdr enum-row)))
                      (foldl
                        (lambda (inner-acc pair)
                          (let ((col (car pair))
                                (digit (cdr pair))
                                (row-cell (/ cell 3))
                                (col-cell (mod cell 3)))
                            (if (and (= row-cell (/ row 3))
                                     (= col-cell (/ col 3)))
                                (cons digit inner-acc) ; add digit to accumulator
                            inner-acc)))
                        outer-acc
                        (enumerate row-digits))))
                  '()
                  (enumerate board))))
    (remove-duplicates digits)))


;; possible? :: [[integer]] -> integer -> integer -> integer -> boolean
(defun possible? (board row col digit)
  "Predicate. Is it possible to enter the given DIGIT in at this position (ROW
and COL) on the BOARD?"
  (not (member digit (remove-duplicates (append
                                          (read-cell-digits board (get-cell row col))
                                          (read-row-digits board row)
                                          (read-col-digits board col))))))


;; set-ref :: [[integer]] -> [[integer]]
(defun set-ref (board row col digit)
  "Set the digit at ROW and COL of BOARD to DIGIT and return the modified board."
  (let ((out-board (copy-sequence board))
        (new-row (copy-sequence (nth row board))))
    (setf (nth col new-row) digit)
    (setf (nth row out-board) new-row)
    out-board))


;; solved? :: [[integer]] -> boolean
(defun solved? (board)
  (not (any?
        (lambda (row)
          (contains? 0 row))
        board)))


;; solve :: [[integer]] -> [[integer]]
(defun solve (board)
  "Solve Sudoku BOARD."
  (catch 'found-solution
    (let ((current-board (copy-sequence board)))
      (when (solved? board)
        (throw 'found-solution current-board)) ; solution

      (for ((row (range 9))
            (col (range 9)))
        (when (zero? (list-ref current-board row col)) ; empty cell
          ; set digit to candidate and solve if solve returns, continue to next candidate
          (for ((candidate valid-digits))
            (progn
              (message (concat "candidate: " (number-to-string candidate) "  row: " (number-to-string row) "  col: " (number-to-string col)))
              (when (possible? current-board row col candidate)
                (let ((updated-board (set-ref current-board row col candidate)))
                  (let ((solved-board (solve updated-board)))
                    (if solved-board
                        (throw 'found-solution solved-board))))))))) ; solution

      nil)))


(solve original-board)


;(run-with-idle-timer 30 t (lambda ()
;                             (with-current-buffer "*Messages*"
;                               (write-file "~/Desktop/emacs-messages.log"))))
