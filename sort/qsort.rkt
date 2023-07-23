#lang racket
(provide (all-defined-out))


;;; QuickSort algorithm ;;;

; swap :: list -> int -> int -> list
(define (swap lst i j)
    (define tmp (list-ref lst i))
    (define out (list-set lst i (list-ref lst j)))
    (list-set out j tmp))

; qsort :: list -> list
(define (qsort lst)
    (cond
        [(empty? lst) lst]
        [(equal? 1 (length lst)) lst]
        [else
            (define piv 0)  ; choose pivot index
            (define pivot (list-ref lst piv))  ; pivot value
            (define end (sub1 (length lst)))  ; end index
            (define ins 0)  ; insertion index
            (define current (swap lst piv end))  ; move pivot to the end
            (for ([i (in-range end)])
                (if (< (list-ref current i) pivot)
                    (begin
                        (set! current (swap current i ins))  ; swap values
                        (set! ins (add1 ins)))               ; increment insertion index
                    (void)))

            ; swap pivot with insertion index
            (set! current (swap current ins end))

            ; recursively sort left and right sublists
            (append (qsort (take current ins))
                    (list pivot)
                    (qsort (drop current (add1 ins))))]))



; QuickSort alternate approach - in-place vector mutation

; swap! :: vector -> int -> int -> null
(define (swap! vec i j)
    (define tmp (vector-ref vec i))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp))

; qsort-vec! :: vector -> null
(define (qsort-vec! vec start end)
    (cond
        [(empty? vec) (void)]
        [(< (- end start) 1) (void)]
        [else
            (define pivot (vector-ref vec start))  ; choose pivot
            (swap! vec start end)  ; move pivot to the end
            (define ins start)  ; insertion index
            (for ([i (in-range start end)])
                (if (< (vector-ref vec i) pivot)
                    (begin
                        (swap! vec i ins)       ; swap values
                        (set! ins (add1 ins)))  ; increment insertion index
                    (void)))
            ; swap pivot with insertion index
            (swap! vec ins end)

            ; recursively sort left and right sides
            (qsort-vec! vec start (sub1 ins))
            (qsort-vec! vec (add1 ins) end)])
    (void))

; qsort2 :: list -> list
(define (qsort2 lst)
    (define vec (list->vector lst))
    (qsort-vec! vec 0 (sub1 (length lst)))
    (vector->list vec))  ; return sorted list



;;; unit tests ;;;

(define test '(5 3 -2 0 1 -2 1 3 5))
(unless (equal? (qsort test) (qsort2 test))
        (error "qsort error: qsort and qsort2 do not match"))

(define test2
    '(-249.016 121.124 -37.366 156.593 -147.875 223.220 36.899 -153.859 42.676 28.080 -72.552
    -54.085 91.863 2.371 133.471 252.705 62.376 -132.541 24.010 -127.511 183.825 185.620 41.171
    13.460 -42.004 225.896 -11.915 -181.012 -170.233 232.589 168.992 -5.371 174.125 -53.697))
(unless (equal? (qsort test2) (qsort2 test2))
        (error "qsort error: qsort and qsort2 do not match"))
