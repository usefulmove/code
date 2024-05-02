#;(define (&& . sexps)
  (cond ((null? sexps) #t)
        ((not (car sexps)) #f)
        (else (apply && (cdr sexps)))))


(define-syntax &&
  (syntax-rules (else if not)
    ((&&) #t)
    ((&& sexp) sexp)
    ((&& sexp rest ...) (if (not sexp)
                               #f
                               (&& rest ...)))))



(display
 (&& (> 3 2)
     (odd? 3)
     (even? 2)))
(newline)
