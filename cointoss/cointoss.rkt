#lang racket


(define (cointoss pattern (total-tosses 1000))
  (let ((generate-toss (lambda (_) (random 2)))
        (not-valid -1))
    (car (foldl
          (lambda (current-toss acc)
            (let* ((pattern-matches (car acc))
                   (toss-minus-2 (cadr acc))
                   (toss-minus-1 (caddr acc))
                   (pattern-to-compare (list toss-minus-2
                                             toss-minus-1
                                             current-toss)))
              (cond ((or (= not-valid
                            toss-minus-2)
                         (= not-valid
                            toss-minus-1)) (list pattern-matches
                                                 toss-minus-1
                                                 current-toss))
                    ((equal? pattern ; pattern matches - reset
                             pattern-to-compare) (list (add1 pattern-matches)
                                                       not-valid
                                                       not-valid))
                    (else (list pattern-matches
                                toss-minus-1
                                current-toss)))))
          '(0 not-valid not-valid) ; (pattern-matches toss-minus-2 toss-minus-1)
          (map
           generate-toss
           (range total-tosses))))))


(cointoss '(0 0 1) 10)
(cointoss '(1 0 1) 10)
