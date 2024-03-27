#lang racket


(define (cointoss pattern (total-tosses 1000))
  (let* ((generate-toss (lambda (_) (random 2)))
         (no-toss -1)
         (total-matches (car (foldl
                              (lambda (current-toss acc)
                                (let* ((pattern-matches (car acc))
                                       (toss-minus-2 (cadr acc))
                                       (toss-minus-1 (caddr acc))
                                       (pattern-to-compare (list toss-minus-2
                                                                 toss-minus-1
                                                                 current-toss)))
                                  (cond ((or (= no-toss
                                                toss-minus-2)
                                             (= no-toss
                                                toss-minus-1)) (list pattern-matches
                                                                     toss-minus-1
                                                                     current-toss))
                                        ((equal? pattern ; pattern matches - reset
                                                 pattern-to-compare) (list (add1 pattern-matches)
                                                                           no-toss
                                                                           no-toss))
                                        (else (list pattern-matches
                                                    toss-minus-1
                                                    current-toss)))))
                              (list 0 no-toss no-toss) ; (pattern-matches toss-minus-2 toss-minus-1)
                              (map
                               generate-toss
                               (range total-tosses))))))
    (exact->inexact (/ total-tosses total-matches))))


(cointoss '(0 1 1) 10000000)
(cointoss '(1 0 1) 10000000)
