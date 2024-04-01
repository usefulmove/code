#|     Scheme basic forms     |#


(if (predicate-expression)
    (then-expression)
    (else-expression))

(cond (((predicate-1) (expression-1))
       ((predicate-2) (expression-2))
       (else (else-expression))))

(and (predicate-1)
     (predicate-2))

(or (predicate-1)
    (predicate-2))

(define variable-name (value-expression))

(define function-name
  (lambda (argument-1 argument-2)
    (expression)))

(let ((variable-1 (expression-1))
      (variable-2 (expression-2)))
  (body-expression))

(begin (expression-1)
       (expression-2)) ; returns result from last expression
