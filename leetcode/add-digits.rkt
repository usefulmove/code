(define (add-digits num)
  (let* ((number->chars (lambda (n) ; int -> [c]
                          (string->list (number->string n))))
         (chars (number->chars num))
         (chars->number (lambda (cs) ; [c] -> int
                          (string->number (list->string cs))))
         (reduce-chars (lambda (cs) ; [c] -> [c]
                         (let* ((char-value (lambda (c) ; [c] -> int
                                              (- (char->integer c)
                                                 (char->integer #\0))))
                                (sum (apply + (map char-value cs))))
                           (number->chars sum))))
         (chars-reduced (reduce-chars chars)))
    (if (= 1 (length chars-reduced))
        (chars->number chars-reduced) ; found solution
        (add-digits (chars->number chars-reduced))))) ; recursive call
