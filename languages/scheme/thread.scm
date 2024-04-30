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

#;(begin
  (display (replace-underscore '(* _ _ _) 8))
  (newline))
#;(begin
  (display (eval (replace-underscore '(* _ _ _) 8)))
  (newline))
#;(begin 
  (display (let ((f (create-lambda '(/ (- (sqrt _) 1) 2))))
             ((eval f) 5)))
  (newline))

; prototype(s)
(begin 
  (display
   (thread 5
    '(sqrt _)
    '(- _ 1)
    '(/ _ 2)
    '(/ _)))
  (newline))

(begin
  (display
   (thread 8
    '(* _ _ _)))
  (newline))

(begin
  (display
   (thread 8
    '(iota (add1 _))
    '(map (lambda (a) (* a a)) _)
    '(apply + _)))
  (newline))
