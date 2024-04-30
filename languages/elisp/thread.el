;; o-thread :: T -> [(T -> T)] -> T
(defun o-thread (seed &rest fns)
  "Thread a SEED value through the function defined by the composition of the
list of functions (FNS). This higher-order function can simplify (and make more
expressive) deeply nested compositional patterns."
  (message (prin1-to-string fns))
  (cond ((null fns) seed)
        (t (let ((f (car fns))
                 (rest (cdr fns)))
             (apply 'o-thread (funcall f seed) rest)))))


(o-thread 5
  'sqrt
  (lambda (a) (- a 1))
  (lambda (a) (/ a 2))) ; 0.6180339887498949
