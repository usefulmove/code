;;; sudoku.el --- Sudoku solver -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: April 9, 2024
;; Modified: April 9, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage:
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: Sudoku solver
;;
;;; Code:


(load-file "~/repos/othello/src/othello.el")


;; core "data types"
;;   board - [int] (length 81)
;;   cell - int (0-80)
;;   row - int (0-8)
;;   col - int (0-8)
;;   box - int (0-8)
;;   val - int (1-9)


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


(setq valid-vals
      (o-range 1 (o-inc 9)))


;; display-board :: board -> nil (impure)
;;               :: [int] -> nil
(defun display-board (board)
  (let ((display-row (lambda (row)
                       (o-for-each
                        (lambda (digit)
                          (message (int-to-string digit))
                          (insert (format " %d " digit)))
                        (get-row-vals board row))
                       (newline))))
    (switch-to-buffer (get-buffer-create "Sudoku"))
    (org-mode)
    (setq show-trailing-whitespace nil)
    (erase-buffer)
    (newline)
    (o-for-each display-row (o-range 9))
    (newline)))


;; get-row-vals :: board -> row -> [vals]
;;              :: [int] -> int -> [int]
(defun get-row-vals (board row)
  )


(display-board original-board)


(o-zip (o-range 8) (make-list 8 0))
