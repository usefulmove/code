;;; sudoku.el --- Sudoku solver -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: April 9, 2024
;; Modified: April 10, 2024
;; Version: 0.0.2
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


(add-to-list 'load-path "~/repos/othello/src/")
(require 'othello)


;; core "data types"
;;   board - [int] (length 81)
;;   cell - int (0-80)
;;   row - int (0-8)
;;   col - int (0-8)
;;   box - int (0-8)
;;   val - int (1-9)


;(setq original-board
;  (list 5 3 0 0 7 0 0 0 0
;        6 0 0 1 9 5 0 0 0
;        0 9 8 0 0 0 0 6 0
;        8 0 0 0 6 0 0 0 3
;        4 0 0 8 0 3 0 0 1
;        7 0 0 0 2 0 0 0 6
;        0 6 0 0 0 0 2 8 0
;        0 0 0 4 1 9 0 0 5
;        0 0 0 0 8 0 0 7 9 ))
(setq original-board
  (list 0 0 0 0 6 2 3 0 0
        3 4 9 0 1 0 7 0 0
        0 5 0 4 3 0 0 0 1
        0 0 2 6 5 0 0 0 9
        0 0 8 0 4 0 1 6 2
        0 6 4 2 9 1 0 3 8
        0 0 0 0 0 6 0 0 0
        0 8 0 0 7 0 0 5 4
        9 0 0 3 2 0 6 0 7 ))


;; backtracking solver
;; solve :: board -> board
;;       :: [int] -> [int]
(defun solve (board)
  (catch 'return
    (let ((empty-cell (find-empty-cell board)))
      (if (= -1 empty-cell) ; if no cells are empty
          board ; return solution
        (o-begin
         (dolist (candidate valid-vals)
           (when (possible? board empty-cell candidate)
             (let ((possible-solution
                    (solve (display-board
                            (set-cell-val board empty-cell candidate)))))
               (sit-for 0.001)
               (when (solved? possible-solution)
                 (throw 'return possible-solution))))) ; return solution (continuation)
         '()))))) ; all candidates exhausted. no solution found.


(setq valid-vals
      (o-range 1 (o-inc 9)))


;; display-board :: board -> board (impure)
;;               :: [int] -> nil
(defun display-board (board)
  (let ((title "           Sudoku Solver")(display-row (lambda (row)
                       (insert "    ")
                       (o-for-each
                        (lambda (val)
                          (insert (if (o-zero-p val)
                                      " _ "
                                    (format " %d " val))))
                        (get-row-vals board row))
                       (newline))))
    (switch-to-buffer (get-buffer-create "Sudoku"))
    (org-mode)
    (display-line-numbers-mode -1)
    (setq show-trailing-whitespace nil)
    (face-remap-add-relative 'default :height 420)
    (erase-buffer)
    (newline)
    (insert title)
    (newline)
    (newline)
    (o-for-each display-row (o-range 9))
    board))


;; get-row-vals :: board -> row -> [val]
;;              :: [int] -> int -> [int]
(defun get-row-vals (board row)
  (o-map 'cadr (o-filter
                (lambda (tup)
                  (let ((cell (car tup)))
                    (= row (get-row cell))))
                (o-zip-with-index board))))


;; find-empty-cell :: board -> cell
;;                 :: [int] -> int
(defun find-empty-cell (board &optional index)
  (unless index (setq index 0))
  (cond ((o-null-p board) -1)
        ((= 0 (car board)) index)
        (o-else (find-empty-cell (cdr board) (o-inc index)))))


; possible? :: board -> cell -> val -> boolean
;           :: [int] -> int -> int -> boolean
(defun possible? (board cell val)
  (not (member val (get-non-candidates board cell))))


; get-non-candidates :: board -> cell -> [val]
;                    :: [int] -> int -> [int]
(defun get-non-candidates (board cell)
  (o-remove-duplicates
   (append (get-row-vals board (get-row cell))
           (get-col-vals board (get-col cell))
           (get-box-vals board (get-box cell)))))


;; get-row-vals :: board -> row -> [val]
;;              :: [int] -> int -> [int]
(defun get-row-vals (board row)
  (let ((matching-tups (o-filter
                        (lambda (tup)
                          (let ((cell (car tup)))
                            (= row (get-row cell))))
                        (o-zip-with-index board))))
    (map 'cadr matching-tups)))


;; get-col-vals :: board -> col -> [val]
;;              :: [int] -> int -> [int]
(defun get-col-vals (board col)
  (let ((matching-tups (o-filter
                        (lambda (tup)
                          (let ((cell (car tup)))
                            (= col (get-col cell))))
                        (o-zip-with-index board))))
    (map 'cadr matching-tups)))


;; get-box-vals :: board -> box -> [val]
;;              :: [int] -> int -> [int]
(defun get-box-vals (board box)
  (let ((matching-tups (o-filter
                        (lambda (tup)
                          (let ((cell (car tup)))
                            (= box (get-box cell))))
                        (o-zip-with-index board))))
    (map 'cadr matching-tups)))


; get-row :: cell -> row
;         :: int -> int
(defun get-row (cell)
  (floor cell 9))


; get-col :: cell -> col
;         :: int -> int
(defun get-col (cell)
    (mod cell 9))


; get-box :: cell -> box
;         :: int -> int
(defun get-box (cell)
  (let ((row (get-row cell))
        (col (get-col cell)))
    (+ (* (floor row 3) 3)
       (floor col 3))))


;; set-cell-val :: board -> cell -> val -> board
;;              :: [int] -> int -> -> int -> [int]
(defun set-cell-val (board cell val)
  (append (o-take cell board)
          (list val)
          (o-drop (o-inc cell) board)))


;; solved? :: board -> boolean
;;         :: [int] -> boolean
(defun solved? (board)
  (and (not (o-null-p board)) ; board is not nil
       (= -1 (find-empty-cell board)))) ; board contains no empty cells


(display-board original-board)

(display-board (solve original-board))
