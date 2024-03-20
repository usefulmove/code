#lang racket



#| Outline

  1. Lists

  2. S-expressions and Evaluation
  
  3. Definitions
  
  4. Functions

  5. Conditional Expressions and Forms

  6. Recursion

  7. Function Composition

  8. Higher-Order Functions

  9. Closures

|#



;; Lists

'(3 1 2)

(list 3 1 2)




;; S-expressions and Evaluation

(/ (- (sqrt 5) 1) 2)  ; 0.6180339887498949

(* pi (sqr 6))  ; 113.09733552923255



;; Definitions

(define lst '(3 1 2 5 4))
lst  ; '(3 1 2 5 4)

(define radius 6)
(* pi (sqr radius))



;; Functions

(+ 3 2)  ; 5

(define square (lambda (n) (* n n)))
(square 16)

(define calculate-area (lambda (r) (* pi (square r))))
(calculate-area radius)

(define snd (lambda (lst) (first (rest lst))))
(define snd (lambda (lst) (car (cdr lst))))
(define snd (lambda (lst) (cadr lst)))

(snd lst)  ; 1

(null? lst)  ; #f
(null? '())  ; #t

(length lst)  ; 5

(reverse lst)  ; '(4 5 2 1 3)

(cons 0 lst)  ; '(0 3 1 2 5 4)

(append lst lst)  ; '(3 1 2 5 4 3 1 2 5 4)



;; Conditional Expressions and Forms

; if, cond
(if (zero? 1)
    "this"
    "that")

(cond ((zero? 1) "this")
      ((equal? 2 2) "that")
      (else "the other thing"))



;; Recursion

; factorial, fibonacci
(todo)



;; Function Composition
(todo)



;; Higher-Order Functions

; map, filter, reduce/fold, any?, all?
(todo)



;; Closures

(define counter
  (lambda (base)
    (let ((current-value base))
      (lambda ()
        (displayln current-value)
        (set! current-value (add1 current-value))))))
