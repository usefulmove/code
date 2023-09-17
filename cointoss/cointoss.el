;;; cointoss.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: September 16, 2023
;; Modified: September 16, 2023
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/dedmonds/cointoss
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(load-file "~/repos/cora/src/cora.el")
(setq lexical-binding t)


(defun make-coins()
  (require 'cl-lib)
  (let ((coins '()))
    (lambda ()
      (setq coins (cons (cl-random 2)
                        (take 2 coins)))
      coins)))


(defun toss-and-check-matches (pattern coins tosses matches)
  (if (= 0 tosses) matches
      (if (equal? pattern (call coins))
          (toss-and-check-matches pattern coins (dec tosses) (inc matches))
          (toss-and-check-matches pattern coins (dec tosses) matches))))


(defun run-simulation (pattern tosses)
  (let ((coins (make-coins)))
    (toss-and-check-matches pattern coins tosses 0)))


;; simulation
(setq max-lisp-eval-depth 1000000)
(setq max-specpdl-size 1000000)

(setq cointosses 10000)
(/ cointosses (run-simulation '(1 0 1) cointosses) 1.0)
(/ cointosses (run-simulation '(0 1 1) cointosses) 1.0)



(provide 'cointoss)
;;; cointoss.el ends here
