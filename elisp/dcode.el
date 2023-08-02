;;; dcode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane
;;
;; Author: Duane <dedmonds@G3>
;; Maintainer: Duane <dedmonds@G3>
;; Created: August 02, 2023
;; Modified: August 02, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
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
;;; dcode.el ends here

; foldl :: (U -> T -> U) -> U -> [T] -> U
(defun foldl (f acc lst)
  (if (null lst)
      acc
      (foldl f (funcall f acc (car lst)) (cdr lst))))

(foldl (lambda (acc a) (+ acc a)) 0 '(3 1 2 5 4))
