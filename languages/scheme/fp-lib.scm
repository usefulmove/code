; thread
(define-syntax thread
  (syntax-rules (eval)
    ((thread seed) seed)
    ((thread seed form)
     (eval (replace-underscore 'form seed)))
    ((thread seed form1 form2 ...)
     (thread (thread seed form1) form2 ...))))

(define (replace-underscore sexp value)
  (cond ((eq? sexp '_) value)
        ((not (pair? sexp)) sexp)
        (else (cons (replace-underscore (car sexp) value)
                    (replace-underscore (cdr sexp) value)))))


; scan
(define (scan-left f lst)
  (let ((last (lambda (lst)
                (car (reverse lst)))))
    (fold-left
     (lambda (acc a) 
       (if (null? acc) (append acc (list a))
           (append acc (list (f a (last acc))))))
     '()
     lst)))


; debug
(define-syntax debug
  (syntax-rules ()
    ((_ sexp) (let ((value sexp))
                (display value)
                (newline)
                value))))
