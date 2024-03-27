#lang racket


(define (cointoss pattern)
  (foldl
    (lambda (current-toss acc)
      (let ((pattern-matches (car acc))
            (previous-tosses (cdr acc)))))
    (0 . (-1 . -1)) ; (pattern matches . (toss - 2 . toss - 1))
    (map (lambda (_) (random 2)) (range 100))))


(cointoss '(0 0 1)) 

(cointoss '(1 0 1)) 
