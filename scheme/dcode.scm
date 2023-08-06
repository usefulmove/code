#lang racket

#|  dcode - a general purpose util library  |#

(provide (all-defined-out))
(require racket/contract)


#| list operations |#

; head :: [T] -> T
(define head car)

; tail :: [T] -> [T]
(define tail cdr)

; init :: [T] -> [T]
(define (init lst)
  (if (null? lst)
      '()
      (reverse (cdr (reverse lst)))))

; last :: [T] -> T  (built-in)

; drop :: number -> [T] -> [T]
(define (drop n lst)
  (if (or (null? lst) (< n 1))
      lst
      (drop (sub1 n) (cdr lst))))

; take :: number -> [T] -> [T]
(define (take n lst)
  (if (or (null? lst) (< n 1))
      null
      (cons (car lst) (take (sub1 n) (cdr lst)))))

; swap :: [T] -> integer -> integer -> [T]
(define (list-swap lst i j)
  (let ([tmp (list-ref lst i)]
        [out (list-set lst i (list-ref lst j))])
    (list-set out j tmp)))

; find index of first instance of an item in a list (returns -1 if not found)
; list-index :: [T] -> T -> integer
(define/contract (list-index item lst [index 0])
  (-> exact-integer? (listof exact-integer?) exact-integer?)
  (if (null? lst)
      -1
      (if (equal? item (car lst))
          index
          (list-index item (cdr lst) (add1 index)))))

; sum :: [T] -> T
(define (sum lst) (apply + lst))

; prod :: [T] -> T
(define (prod lst) (apply * lst))

; display-list :: [T] -> null  ( side effects only )
(define (display-list lst)
  (for-each displayln lst))

#| higher-order functions |#

; any? :: [T] -> (T -> boolean) -> boolean
(define (any? f lst)
  (cond [(null? lst) #f]
        [(f (car lst)) #t]
        [else (any? f (cdr lst))]))

; all? :: [T] -> (T -> boolean) -> boolean
(define (all? f lst)
  (cond [(null? lst) #t]
        [(not (f (car lst))) #f]
        [else (all? f (cdr lst))]))

; reduce :: (U -> T -> U) -> U -> [T] -> U
; reverses the argument order of the foldl primitive
(define (reduce f acc lst) (if (null? lst)
                               acc
                               (reduce f acc (f (car lst)) (cdr lst))))



#| currying |#

; curry2 :: (T -> U -> V) -> (T -> (U -> V))
(define (curry2 f)
  (lambda (a) (lambda (b) (f a b))))

; curry3 :: (T -> U -> V -> W) -> (T -> (U -> (V -> W)))
(define (curry3 f)
  (lambda (a) (lambda (b) (lambda (c) (f a b c)))))



; iota :: integer -> [integer]
(define (iota n) (range 1 (add1 n)))

; atom? :: object -> boolean
(define (atom? obj)
  (and (not (null? obj))
       (not (pair? obj))))

; fixed-point convergence function
; converge-fixed-point :: (number -> number) -> number -> number
(define/contract (converge-fixed-point f guess)
  (-> (-> number? number?) number? number?)
  (letrec
    ([epsilon 0.000000001]
     [close-enough? (lambda (a b) (> epsilon (abs (- a b))))]
     [improve-guess (lambda (guess)
                        (cond [(close-enough? guess (f guess)) (f guess)]
                              [else (improve-guess (f guess))]))])
      (improve-guess guess)))



#| debug helper functions |#

; debug-print :: T -> null  ( side effect only )
(define-syntax-rule (debug-print var)
  (begin (displayln (list 'debug: 'var 'is var))))

; debug-args
(define (debug-args . args)
  (for-each displayln args))



#| unit tests |#

(unless (equal? (iota 8) '(1 2 3 4 5 6 7 8))
  (error "error (dcode): iota unit test failed"))

(unless (let ([golden-ratio-est (converge-fixed-point
                                    (lambda (a) (/ (add1 (sqr a)) (add1 (* a 2))))
                                    1.0)]
            [golden-ratio (/ (- (sqrt 5) 1) 2)])
        (< (abs (- golden-ratio-est golden-ratio)) 0.001))
  (error "error (dcode): converge-fixed-point unit test failed"))

(unless (equal? #t (any? identity '(#f #t #f)))
  (error "error (dcode): any? unit test failed"))

(unless (equal? #f (all? identity '(#f #t #f)))
  (error "error (dcode): all? unit test failed"))
