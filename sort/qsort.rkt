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
        [(< (- end start) 2) (void)]
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

(qsort (list 3 1 2 5 4 0))
(qsort2 (list 3 1 2 5 4 0))

(qsort (list 5 3 -2 0 1 -2 1 3 5))
(qsort2 (list 5 3 -2 0 1 -2 1 3 5))
