; thread
(define-syntax thread
  (syntax-rules ()
    ((thread seed) seed)
    ((thread seed form more ...)
     (thread ((eval (create-lambda form)) seed) more ...))))

(define (create-lambda sexp)
  `(lambda (_arg_)
     ,(replace-underscore sexp '_arg_)))

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
