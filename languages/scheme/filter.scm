(define (_filter keep? lst)
  (if (null? lst)
      '()
      (let ((head (car lst))
            (rest (cdr lst)))
        (if (keep? head)
            (cons head
                  (_filter keep? rest))
            (_filter keep? rest)))))

(display
 (_filter
  (lambda (a) (zero? (mod a 3)))
  (iota (add1 99))))
(newline)
