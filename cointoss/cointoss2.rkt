#lang racket

(define (cointoss pattern tosses)
  (let ((toss (lambda () (random 2)))
        (pattern-length (length pattern)))
    (let loop ((current-toss tosses)
               (history '())
               (matches 0.0))
      (cond ((zero? current-toss) ; done
             (if (zero? matches)
                 -1
                 (/ tosses matches))) ; analyze
            ((< (length history) pattern-length)
             (loop (- current-toss 1)
                   (cons (toss) history)
                   matches))
            ((equal? pattern history) ; match
             (loop (- current-toss 1)
                   (list (toss)) ; reset
                   (+ matches 1)))
            (else
             (loop (- current-toss 1)
                   (cons (toss) (take history (- pattern-length 1)))
                   matches))))))

(let ((tosses 8000000)
      (show (lambda (n)
              (displayln (/ (round (* n 100)) 100.0)))))
  (show (cointoss '(0 0 0) tosses))
  (show (cointoss '(0 1 0) tosses))
  (show (cointoss '(0 0 1) tosses)))
