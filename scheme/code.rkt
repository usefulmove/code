#lang racket

;; misc
(define (init lst)
    (if (null? lst)
        '()
        (reverse (cdr (reverse lst)))))

(init '(3 1 2 5 4))  ; '(3 1 2 5)


(define (display-list lst)
    (for-each displayln lst))

(display-list '(3 1 2))
; 3
; 1
; 2


(define (swap lst i j)
    (define tmp (list-ref lst i))
    (define out (list-set lst i (list-ref lst j)))
    (list-set out j tmp))

(swap '(3 1 2 5 4) 1 3)  ; '(3 5 2 1 4)


(define (square-root n)
    (define epsilon 0.000000001)
    (define (improve-guess guess)
        (cond
            [(> epsilon (abs (- n (* guess guess)))) guess]
            [else (improve-guess (/ (+ guess (/ n guess)) 2))]))
    (cond
        [(< n 0) (error "square-root of negative number")]
        [else (improve-guess 1.0)]))

(square-root 618)  ; 24.859605789312106


;; functional programming

; function composition
(define ())


; threading


; filter map reduce
(define (square a) (* a a))
(apply + (map square (filter odd? seq)))

(- (sqrt 10))
((compose - sqrt) 10)

(define (f seq) (filter odd? seq))
(define (m seq) (map square seq))
(define (r seq) (apply + seq))
(define fmr (compose r m f))
(fmr seq)
