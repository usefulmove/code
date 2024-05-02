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


(begin 
  (display
   (thread 5
    (sqrt _)
    (- _ 1)
    (/ _ 2)))
  (newline))
