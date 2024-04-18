(define (life-path num)
  (let* ((reduce (lambda (n)
                   (fold-left
                    (lambda (acc char)
                      (+ acc (- (char->integer char)
                                (char->integer #\0))))
                    0
                    (string->list (number->string n)))))
         (reduced (reduce num)))
    (if (< reduced 10)
        reduced
        (life-path reduced))))


(display
 (life-path 06181985))
(newline)
