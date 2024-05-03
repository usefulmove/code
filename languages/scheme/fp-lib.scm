;; macro support

(define (create-lambda sexp)
  `(lambda (_arg_)
     ,(replace-underscore sexp '_arg_)))


(define (replace-underscore sexp value)
  (cond ((eq? sexp '_) value)
        ((not (pair? sexp)) sexp)
        (else (cons (replace-underscore (car sexp) value)
                    (replace-underscore (cdr sexp) value)))))


;; thread
(define-syntax thread
  (syntax-rules (eval)
    ((thread seed) seed)
    ((thread seed form more ...)
     (thread ((eval (create-lambda 'form)) seed) more ...))))


;; fn (function)
(define-syntax fn
  (syntax-rules (eval)
    ((_ form) (eval (create-lambda 'form)))))


;; red (reduce)
(define-syntax red
  (syntax-rules (eval fold-left)
    ((_ form seed lst)
     (fold-left
      (eval `(lambda (:acc _) form))
      seed
      lst))))


;; enumerate
(define (enumerate lst . params)
  (let ((index (if (not (null? params)) (car params) 0)))
    (cond ((null? lst) '())
          (else (cons (cons index
                            (car lst))
                      (enumerate (cdr lst) (+ 1 index)))))))


;; scan
(define (scan-left f lst)
  (let ((last (lambda (lst)
                (car (reverse lst)))))
    (fold-left
     (lambda (acc a) 
       (if (null? acc) (append acc (list a))
           (append acc (list (f a (last acc))))))
     '()
     lst)))


;; debug
(define-syntax debug
  (syntax-rules (display let newline)
    ((_ sexp) (let ((value sexp))
                (display value)
                (newline)
                value))))
