(define fold-left
  (lambda (f acc lst)
    (cond ((null? lst) acc)
          (else (fold-left f (f acc (car lst)) (cdr lst))))))
