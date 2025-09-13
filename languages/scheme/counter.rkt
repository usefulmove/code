#lang racket

(define counter (lambda (lst)
                  (let loop ((cs lst)
                             (cnts '())) ; assoc list
                    (if (empty? cs)
                        (reverse cnts)
                        (let* ((c (car cs))
                               (ind (index-of (map car cnts) c)))
                          (loop (cdr cs)
                                (if ind
                                    (append (take cnts ind)
                                            (let ((c-cnt (cdr
                                                          (list-ref cnts ind))))
                                              (list (cons c (+ c-cnt 1))))
                                            (drop cnts (+ ind 1)))
                                    (cons (cons c 1) cnts))))))))

(counter (string->list "this is a test.")) ; =>
; '((#\t . 3) (#\h . 1) (#\i . 2) (#\s . 3) (#\space . 3) (#\a . 1) (#\e . 1) (#\. . 1))
