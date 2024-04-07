;; Luhn algorithm

(load-file "~/repos/othello/src/othello.el")

(defun luhn (s)
  (let* ((digits (map
                  (lambda (c) (char-to-int c))
                  (string-to-list s)))
         (sum
          (cdr (o-fold-right
                (lambda (acc digit)
                  (let ((index (car acc))
                        (total (cdr acc))
                        (double-and-set (lambda (n)
                                         (let ((double (+ n n)))
                                           (if (> double 9)
                                               (- double 9)
                                             double)))))
                    (if (even? index)
                        (cons (inc index)
                              (+ total digit))
                      (cons (inc index)
                            (+ total (call double-and-set digit))))))
                '(0 . 0) ; (index . total)
                digits))))
    (o-zerop (mod sum 10))))

(luhn "79927398713")
