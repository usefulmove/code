;;; cointoss.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: September 16, 2023
;; Modified: September 16, 2023
;; Version: 0.0.2
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
    (lambda (cmd)
      (cond ((equal? :flip cmd) (setq coins (cons (cl-random 2)
                                              (take 2 coins))))
            ((equal? :reset cmd) (setq coins '())))
      coins)))


(defun toss-and-check-matches (pattern coins tosses)
  (let ((matches 0))
    (dotimes (i tosses)
      (when (equal? pattern (call coins :flip))
        (setq matches (inc matches))
        (call coins :reset)))
    matches))


(defun run-simulation (pattern tosses)
  (let ((coins (make-coins)))
    (toss-and-check-matches pattern coins tosses)))


;; simulation
(setq cointosses 1000000)
(/ cointosses (run-simulation '(1 0 1) cointosses) 1.0) ; 9.983427510332847
(/ cointosses (run-simulation '(0 1 1) cointosses) 1.0) ; 7.980145398249156



(provide 'cointoss)
;;; cointoss.el ends here
