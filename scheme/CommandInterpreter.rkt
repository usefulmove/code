#lang racket

;; unary operations
(define (sqrt-op x) (sqrt x))
(define (inv-op x) (/ 1 x))

;; binary operations
(define (add-op x y) (+ x y))
(define (sub-op x y) (- x y))
(define (mul-op x y) (* x y))
(define (div-op x y) (/ x y))

;; stack operations
(define (dup-op lst) (cons (first lst) lst))

;; dispatch table for operations
(define operation-table
  (hash "sqrt" sqrt-op
        "inv" inv-op
        "+" add-op
        "-" sub-op
        "*" mul-op
        "/" div-op
        "dup" dup-op))

;; function to get operation from table
(define (get-operation op)
  (hash-ref operation-table op (lambda () (error "unknown operation"))))

;; function to determine if a string is numeric
(define (numeric? str)
  (regexp-match #px"^[+-]?([0-9]*[.])?[0-9]+$" str))

;; dispatching function
(define (dispatch op stack)
  (cond [(numeric? op) (cons (string->number op) stack)]
        [(= (length stack) 1)
         (if (string=? op "dup")
             (dup-op stack)
             (cons ((get-operation op) (first stack)) (rest stack)))]
        [(= (length stack) 2)
         (if (or (string=? op "dup") (string=? op "sqrt") (string=? op "inv"))
             (cons ((get-operation op) (first stack)) (cons (second stack) (cddr stack)))
             (cons ((get-operation op) (second stack) (first stack)) (cddr stack)))]
        [else stack]))

;; evaluates a list of operations
(define (evaluate-ops ops)
  (let loop ([remaining-ops ops] [stack '()])
    (if (null? remaining-ops)
        stack
        (loop (rest remaining-ops) (dispatch (first remaining-ops) stack)))))

(define (parse s-expr)
  (string-split s-expr))

(define (main)
  ;; S-expression to be evaluated
  (define s-expr "5 sqrt 1 - 2 / dup inv")

  ;; parse and evaluate the s-expression
  (displayln (evaluate-ops (parse s-expr))))

(main)
