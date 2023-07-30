#lang racket

;; miscellaneous ;;

; init :: [T] -> [T]
(define (init lst)
    (if (null? lst)
        '()
        (reverse (cdr (reverse lst)))))

(init '(3 1 2 5 4))  ; '(3 1 2 5)


; macros
(define-syntax-rule (head lst) (car lst))
(define-syntax-rule (tail lst) (cdr lst))

(define-syntax-rule (1st lst) (car lst))
(define-syntax-rule (2nd lst) (car (cdr lst)))
(define-syntax-rule (end lst) (last lst))


; display-list :: [T] -> null  ( side effect only )
(define (display-list lst)
    (for-each displayln lst))

(display-list '(3 1 2))
; 3
; 1
; 2


; swap :: [T] -> Integer -> Integer -> [T]
(define (swap lst i j)
    (define tmp (list-ref lst i))
    (define out (list-set lst i (list-ref lst j)))
    (list-set out j tmp))

(swap '(3 1 2 5 4) 1 3)  ; '(3 5 2 1 4)


; square-root :: Number -> Number
(define (square-root n)  ; example only (Babylonian method) - use (sqrt n) primative
    (define epsilon 0.000000001)
    (define (improve-guess guess)
        (cond [(> epsilon (abs (- n (* guess guess)))) guess]
              [else (improve-guess (/ (+ guess (/ n guess)) 2))]))
    (cond [(< n 0) (error "square-root of negative number")]
          [else (improve-guess 1.0)]))

(square-root 618)  ; 24.859605789312106


; fixed-point convergence numerical method
; converge-fixed-point :: (Number -> Number) -> Number -> Number
(define (converge-fixed-point f guess)
    (define (close-enough? a b)
        (define epsilon 0.000000001)
        (> epsilon (abs (- a b))))
    (define (improve-guess guess)
        (cond [(close-enough? guess (f guess)) (f guess)]
              [else (improve-guess (f guess))]))
    (improve-guess guess))

(converge-fixed-point
    (lambda (a) (/ (+ a (/ 618 a)) 2))
    1.0)  ; 24.859605789312106

; alternate square root function using fixed-point convergence
(define (square-root2 n)
    (converge-fixed-point
        (lambda (a) (/ (+ a (/ n a)) 2))
        1.0))

(square-root2 618)  ; 24.859605789312106

; cube root using fixed-point convergence
(define (cube-root n)
    (converge-fixed-point
        (lambda (a) (/ (+ (/ n (sqr a)) (* a 2)) 3))
        1.0))

(cube-root 512) ; 8.0

; golden ratio using fixed-point convergence
(define (golden-ratio)
    (converge-fixed-point
        (lambda (a) (/ (add1 (sqr a)) (add1 (* a 2))))
        1.0))

(golden-ratio)  ; 0.6180339887498949



;; regular expressions ;;

(regexp-match? #rx"needle" "hay needle stack")  ; #t

(regexp-replace* #rx"[a-c]" "drracket" string-upcase)  ; "drrACket"

(regexp-split #rx" +" "one  two   three")  ; '("one" "two" "three")


;; functional programming ;;

; higher-order functions
(define seq '(1 2 3 4 5 6 7 8))

(filter odd? seq)  ; '(1 3 5 7)
(map sqr seq)  ; '(1 4 9 16 25 36 49 64)
(foldl + 0 seq)  ; 36


; currying
; add :: Integer -> (Integer -> Integer)
(define (add a)
    (lambda (b) (+ a b)))

((add 2) 3)  ; 5

(define add2 (add 2))
(add2 3)  ; 5


; function composition
(define seq '(1 2 3 4 5 6 7 8))

(apply + (map sqr (filter odd? seq)))  ; 84

(define (f lst) (filter odd? lst))
(define (g lst) (map sqr lst))
(define (h lst) (apply + lst))
(define composed-f (compose h g f))
(composed-f seq)  ; 84


; threading
(require threading)
(~> seq f g h)  ; 84


; for loop
(for ([i (range 1 9)])
    (displayln (* i i)))


; list comprehension
(for/list ([a (range 1 9)])
    (* a a))


;;; aliases ;;;

(define list. (lambda args args))

(define list-copy.
    (lambda (lst) (if (null? lst)
                      '()
                      (cons (car lst) (list-copy. (cdr lst))))))

(define map.
    (lambda (f lst) (if (null? lst)
                        '()
                        (cons (f (car lst)) (map. f (cdr lst))))))

(define filter.
    (lambda (f lst) (if (null? lst)
                        '()
                        (if (f (car lst))
                            (cons (car lst) (filter. f (cdr lst)))
                            (filter. f (cdr lst))))))

(define reduce.
    (lambda (f acc lst) (if (null? lst)
                            acc
                            (reduce. f (f (car lst) acc) (cdr lst)))))

(define apply.
    (lambda (f lst) (cond [(null? lst) '()]
                          [(equal? 1 (length lst)) (car lst)]
                          [else (f (car lst) (apply. f (cdr lst)))])))


; closure
(define counter
    (lambda ()
        (let ([count 0])
          (lambda () (set! count (add1 count)) count))))

(define c1 (counter))
(c1) ; 1
(define c2 (counter))
(c1) ; 2
(c1) ; 3
(c2) ; 1


;;; data abstraction ;;;
(define cons.
    (lambda (a b)
        (lambda (symbl) (cond [(equal? symbl 'car.) a]
                              [(equal? symbl 'cdr.) b]))))

(define pair. (cons. 2 3))

(pair. 'car.)  ; 2
(pair. 'cdr.)  ; 3

((cons. 0 pair.) 'car.)  ; 0
(((cons. 0 pair.) 'cdr.) 'car.)  ; 2
(((cons. 0 pair.) 'cdr.) 'cdr.)  ; 3



(= 8 8 8)  ; #t
