#lang racket

(define (add-digits num)
  (let* ((char->value (lambda (c)
                        (- (char->integer c)
                           (char->integer #\0))))
         (digits (map
                   char->value
                   (string->list (number->string num))))
         (sum (lambda (lst)
                (apply + lst)))
         (total (sum digits)))
    (if (< total 10)
        total
        (add-digits total))))

(add-digits 38)
(add-digits 0)
