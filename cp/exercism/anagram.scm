(import (rnrs))

(define (anagram target words)
  (fold-left
   (lambda (acc word)
     (let ((normalise (lambda (word)
                        (sort char<? (map
                                      char-downcase
                                      (string->list word))))))
       (if (and (not (equal? (string-downcase target)
                             (string-downcase word)))
                (equal? (normalise target)
                        (normalise word)))
           (cons word acc) ; add anagram to accumulator
           acc)))
   '()
   words))
