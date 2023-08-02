;;; dcode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 02, 2023
;; Modified: August 02, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/dedmonds/dcode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(provide 'dcode)

; foldl :: (U -> T -> U) -> U -> [T] -> U
(defun foldl (f acc lst)
  (if (null lst)
      acc
      (foldl f (funcall f acc (car lst)) (cdr lst))))

; map :: (T -> T) -> [T] -> [T]
(defun map (f lst)
  (if (null lst)
      lst
      (cons (funcall f (car lst)) (map f (cdr lst)))))

; filter :: (T -> boolean) -> [T] -> [T]
(defun filter (f lst)
  (if (null lst)
      lst
      (if (not (funcall f (car lst)))
          (filter f (cdr lst))
          (cons (car lst) (filter f (cdr lst))))))

; even? :: number -> booleean
(defun even? (n) (= 0 (mod n 2)))
;
; odd? :: number -> booleean
(defun odd? (n) (not (even? n)))

(foldl (lambda (acc a) (+ acc a)) 0 (map (lambda (a) (* a a)) '(1 2 3 4 5 6 7 8)))  ; 204

;;; dcode.el ends here
