#lang racket

(define (luhn s)
  (let ((chars (string->list s)))
    (cond ((ormap
            (lambda (c)
              (or (and (not (char-numeric? c))
                       (not (equal? #\space c)))))
            chars) #f)
          (else (let* ((digits (filter-map
                                (lambda (c)
                                  (if (char-numeric? c)
                                      (- (char->integer c)
                                         (char->integer #\0))
                                      #f))
                                chars))
                       (sum (cdr (foldr
                                  (lambda (digit acc)
                                    (let ((index (car acc))
                                          (total (cdr acc))
                                          (double-and-set (lambda (n)
                                                            (let ((double (+ n n)))
                                                              (if (> double 9)
                                                                  (- double 9)
                                                                  double)))))
                                      (if (even? index)
                                          (cons (add1 index)
                                                (+ total digit))
                                          (cons (add1 index)
                                                (+ total (double-and-set digit))))))
                                  '(0 . 0) ; (index . total)
                                  digits))))
                  (if (< (length digits) 2)
                      #f
                      (zero? (modulo sum 10))))))))

(luhn "79927398713")
