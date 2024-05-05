;;; collatz-conjecture.el --- Collatz Conjecture (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun steps (number &rest args)
  "Count the steps to reach 1 using the Collatz conjecture."
  (let ((acc (if (null args)
                 0
               (car args))))
    (cond ((or (< number 1)
               (not (integerp number))) (error "invalid input"))
          ((= 1 number) acc)
          ((= 0 (mod number 2)) (steps (/ number 2) (+ 1 acc)))
          ((= 1 (mod number 2)) (steps (+ 1 (* 3 number)) (+ 1 acc))))))

(provide 'collatz-conjecture)
;;; collatz-conjecture.el ends here
