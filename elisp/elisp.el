;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atoms

3

pi

"strings are atoms"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; procedures (built-in)

(reverse "scream") ; compare with upcase("scream")

(message "hello")

(insert " ; inserted text")

(+ 3 2)

(* 2 2 2)

(/ (+ 8 8) (- 4 2)) ; compound procedure

(/ (+ 8 8)  ; structure is not important - you could put everything in one line
   (- 4 2)) ; but don't do that




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lists (Lisp = list processor)

(3 1 2)

(quote (3 1 2))

'(3 1 2)

'(1 (2 3) (4 5) 6 ((7 8))) ; nested list


'() ; empty list (nil)

(null '())

(null nil)


(car '(3 1 2)) ; "first"

(cdr '(3 1 2)) ; "rest"

(cdr '(8))

(car (cdr '(3 1 2)))

(cadr '(3 1 2))

(cons 1 '(2 3)) ; "construct"


(append '(0 1 2) '(3 4))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symbols (variables)

(setq orange 8)

orange

(+ orange orange)

(setq lst '(3 1 2))

(car lst)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local scope

(let ((six 6)
      (eight 8))
  (* six eight))

six



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom procedures

(defun square (n)
  (* n n))

(square 8)


(defun say-hello ()
  (message "hello there"))

(say-hello)


(defun vector-length (x y) ; compound procedure - calculate length of vector
  (sqrt (+ (square x)
           (square y))))

(vector-length 3 4)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditionals

(if t 1 2)
(if nil 1 2)

(if (= 4 (+ 2 2))
    "great"
    "oh fuck")

(cond ((= 4 (+ 2 2)) "great")
      (t "oh fuck"))

(let ((a 3))
  (cond ((= a 1) "one")
        ((= a 2) "two")
        ((= a 3) "three")
        (t "something else")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursion

(defun factorial (n)
  (if (= 0 n)
      1
      (* n (factorial (- n 1)))))

(factorial 3)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anonymous functions

(lambda (a b) (+ a b))

((lambda (a b) (+ a b)) 3 2)

(fset 'cube (lambda (n) (* n n n)))

(cube 8)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; higher-order functions

(mapcar 'reverse '("apply" "function" "to" "all" "elements" "in" "list")) ; map

(cl-remove-if-not 'cl-oddp '(3 1 2 5 4)) ; filter

(cl-reduce (lambda (a b) (+ a b)) '(3 1 2)) ; reduce
