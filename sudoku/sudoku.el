(load-file "~/repos/cora/src/cora.el")


(setq original-board '((0 0 1  8 0 0  0 0 0)
                       (6 7 0  2 0 3  0 1 0)
                       (2 0 5  7 0 0  6 0 3)

                       (3 5 0  6 2 0  8 0 0)
                       (7 6 2  0 5 8  3 0 0)
                       (0 0 0  0 0 4  0 5 0)

                       (0 9 0  5 8 6  0 0 0)
                       (5 0 0  9 0 0  7 0 0)
                       (0 2 6  4 3 0  5 9 0)))



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


;; read-col-values :: [[integer]] -> [integers]
(defun read-col-values (board col)
  "Return the set of values assigned to the column (COL) on the BOARD."
  (let ((values (foldl
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
    (remove-duplicates values)))


;; read-row-values :: [[integer]] -> [integers]
(defun read-row-values (board row)
  "Return the set of values assigned to the ROW on the BOARD."
  (let ((values (list-ref board row)))
    (remove-duplicates values)))


;; read-cell-values :: [[integer]] -> [integers]
(defun read-cell-values (board cell)
  "Return the set of values assigned to the CELL on the BOARD."
  (let ((values (foldl
                  (lambda (outer-acc enum-row)
                    (let ((row (car enum-row))
                          (row-values (cdr enum-row)))
                      (foldl
                        (lambda (inner-acc pair)
                          (let ((col (car pair))
                                (value (cdr pair))
                                (row-cell (/ cell 3))
                                (col-cell (mod cell 3)))
                            (if (and (= row-cell (/ row 3))
                                     (= col-cell (/ col 3)))
                                (cons value inner-acc) ; add value to accumulator
                            inner-acc)))
                        outer-acc
                        (enumerate row-values))))
                  '()
                  (enumerate board))))
    (remove-duplicates values)))


;; possible? :: [[integer]] -> integer -> integer -> integer -> boolean
(defun possible? (board row col value)
  "Predicate. Is it possible to enter the given VALUE in at this position (ROW
and COL) on the BOARD?"
  (not (member value (remove-duplicates (append
                                          (read-cell-values board (get-cell row col))
                                          (read-row-values board row)
                                          (read-col-values board col))))))


;; set-ref :: [[integer]] -> [[integer]]
(defun set-ref (board row col value)
  "Set the value at ROW and COL of BOARD to VALUE and return the modified board."
  (let ((out-board (copy-sequence board))
        (new-row (copy-sequence (nth row board))))
    (setf (nth col new-row) value)
    (setf (nth row out-board) new-row)
    out-board))


;; solve :: [[integer]] -> [[integer]]
(defun solve (board)
  "Solve Sudoku BOARD."
  (catch 'found-solution
    (let ((current-board (copy-sequence board)))
      (unless (any? ; guard
                (lambda (row)
                  (contains? 0 row))
                current-board) ; check that there are no zeros
        (throw 'found-solution current-board)) ; solution found

      (for ((row (range 9))
            (col (range 9)))
        (when (zero? (list-ref current-board row col)) ; empty cell
          ; set value to candidate and solve if solve returns, continue to next candidate
          (for ((candidate (range 1 (inc 9))))
            (progn
              (message (concat "candidate:" (number-to-string candidate) " row: " (number-to-string row) " col: " (number-to-string col)))
              (when (possible? current-board row col candidate)
                (let ((updated-board (set-ref current-board row col candidate)))
                  (let ((solved-board (solve updated-board)))
                    (if solved-board
                        (throw 'found-solution solved-board)))))))))

      nil)))


(solve original-board)


;(run-with-idle-timer 30 t (lambda ()
;                             (with-current-buffer "*Messages*"
;                               (write-file "~/Desktop/emacs-messages.log"))))
