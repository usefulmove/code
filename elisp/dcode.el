;;; dcode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 02, 2023
;; Modified: August 03, 2023
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

; any :: (T -> boolean) -> [T] -> boolean
(defun any (f lst)
  (if (null lst)
      nil
      (if (funcall f (car lst))
          t
          (any f (cdr lst)))))

; all :: (T -> boolean) -> [T] -> boolean
(defun all (f lst)
  (if (null lst)
      t
      (if (not (funcall f (car lst)))
          nil
          (all f (cdr lst)))))


; even :: number -> booleean
(defun even (n) (= 0 (mod n 2)))

; odd :: number -> booleean
(defun odd (n) (not (even n)))

(provide 'dcode)

;;; dcode.el ends here
