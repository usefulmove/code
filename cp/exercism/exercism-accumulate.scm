(define (accumulate f xs)
  (cond ((null? xs) '())
        (else (cons (f (car xs))
                    (accumulate f (cdr xs)))))) 


(accumulate (lambda (a) (* a a)) '(1 2 3)) 
(accumulate string-upcase '("this" "that" "other"))
(accumulate (lambda (s) (list->string (reverse (string->list s)))) '("this" "that" "other")) 
