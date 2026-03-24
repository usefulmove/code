# Functional Programming in Racket

Racket is a descendant of Scheme and a dialect of Lisp, making it inherently functional. It emphasizes immutability, first-class functions, and recursive data processing.

## Core FP Concepts in Racket

### 1. Immutability
```racket
; Lists are immutable by default
(define numbers '(1 2 3 4 5))
; There's no way to mutate this list

; cons creates a new list
(define extended (cons 0 numbers))  ; '(0 1 2 3 4 5)
; numbers is unchanged: '(1 2 3 4 5)

; append creates a new list
(define combined (append numbers '(6 7 8)))  ; '(1 2 3 4 5 6 7 8)

; Structs can be immutable (default) or mutable
(struct point (x y))  ; Immutable by default
(define p (point 3 4))
; (set-point-x! p 5)  ; Error! No mutator exists

; Mutable struct (avoid when possible)
(struct mpoint (x y) #:mutable)
(define mp (mpoint 3 4))
(set-mpoint-x! mp 5)  ; Works, but not functional style
```

### 2. Pure Functions
```racket
; Pure function: same input → same output, no side effects
(define (square x)
  (* x x))

; Pure function with multiple arguments
(define (hypotenuse a b)
  (sqrt (+ (square a) (square b))))

; Not pure (has side effects)
(define counter 0)
(define (impure-square x)
  (set! counter (add1 counter))  ; Side effect!
  (* x x))

; Not pure (depends on external state)
(define (impure-random x)
  (+ x (random 10)))  ; Non-deterministic!
```

### 3. First-Class Functions
```racket
; Functions can be passed as arguments
(define (apply-twice f x)
  (f (f x)))

(apply-twice add1 5)  ; 7
(apply-twice sqr 2)   ; 16

; Lambda (anonymous functions)
(define square (lambda (x) (* x x)))
; Shorthand
(define square (λ (x) (* x x)))

; Functions can return functions
(define (make-adder n)
  (λ (x) (+ x n)))

(define add5 (make-adder 5))
(define add10 (make-adder 10))

(add5 3)   ; 8
(add10 3)  ; 13

; Functions stored in data structures
(define operations (list add1 sub1 sqr))
(map (λ (f) (f 5)) operations)  ; '(6 4 25)
```

---

## Essential List Functions

### map
```racket
(define numbers '(1 2 3 4 5))

; Basic map
(map sqr numbers)  ; '(1 4 9 16 25)

; With lambda
(map (λ (x) (* x 2)) numbers)  ; '(2 4 6 8 10)

; Map with multiple lists (like zip-with)
(map + '(1 2 3) '(10 20 30))  ; '(11 22 33)
(map * '(1 2 3) '(2 2 2) '(10 10 10))  ; '(20 40 60)

; Map over strings (as lists of chars)
(map char-upcase (string->list "hello"))  ; '(#\H #\E #\L #\L #\O)
(list->string (map char-upcase (string->list "hello")))  ; "HELLO"
```

### filter
```racket
(define numbers '(1 2 3 4 5 6 7 8 9 10))

; Basic filter
(filter even? numbers)  ; '(2 4 6 8 10)
(filter odd? numbers)   ; '(1 3 5 7 9)

; With lambda
(filter (λ (x) (> x 5)) numbers)  ; '(6 7 8 9 10)

; Filter with predicate function
(define (positive? x) (> x 0))
(filter positive? '(-2 -1 0 1 2))  ; '(1 2)

; filter-not (opposite of filter)
(filter-not even? numbers)  ; '(1 3 5 7 9)

; partition (split into two lists)
(partition even? numbers)  ; '((2 4 6 8 10) (1 3 5 7 9))
```

### foldl / foldr (reduce)
```racket
(define numbers '(1 2 3 4 5))

; foldl - left fold (accumulator first in lambda)
(foldl + 0 numbers)  ; 15 (sum)
(foldl * 1 numbers)  ; 120 (product)

; foldl with explicit lambda
(foldl (λ (x acc) (+ acc x)) 0 numbers)  ; 15

; foldr - right fold
(foldr + 0 numbers)  ; 15

; Difference between foldl and foldr
(foldl cons '() '(1 2 3))  ; '(3 2 1) - reverses!
(foldr cons '() '(1 2 3))  ; '(1 2 3) - preserves order

; Build string
(foldl (λ (word acc) 
         (if (string=? acc "") 
             word 
             (string-append acc " " word)))
       ""
       '("hello" "world" "racket"))  ; "hello world racket"

; Implementing map with foldr
(define (my-map f lst)
  (foldr (λ (x acc) (cons (f x) acc)) '() lst))
```

### flatten / append-map (flatMap)
```racket
; flatten - deep flatten
(flatten '((1 2) (3 (4 5)) 6))  ; '(1 2 3 4 5 6)

; append-map - flatMap equivalent
(append-map (λ (x) (list x (* x 10))) '(1 2 3))
; '(1 10 2 20 3 30)

; Practical example: get all elements from nested structure
(define users
  '((alice (admin user))
    (bob (user))
    (charlie (admin moderator user))))

(append-map cadr users)  ; '(admin user user admin moderator user)

; Remove duplicates with remove-duplicates
(remove-duplicates (append-map cadr users))  ; '(admin user moderator)
```

### take / drop
```racket
(define numbers '(1 2 3 4 5 6 7 8 9 10))

(take numbers 3)  ; '(1 2 3)
(drop numbers 3)  ; '(4 5 6 7 8 9 10)

(take-right numbers 3)  ; '(8 9 10)
(drop-right numbers 3)  ; '(1 2 3 4 5 6 7)

; takef / dropf - with predicate
(takef '(2 4 6 7 8 10) even?)  ; '(2 4 6)
(dropf '(2 4 6 7 8 10) even?)  ; '(7 8 10)

; splitf-at - split at first failure
(splitf-at '(2 4 6 7 8 10) even?)  ; '((2 4 6) (7 8 10))

; split-at - at index
(split-at numbers 5)  ; '((1 2 3 4 5) (6 7 8 9 10))
```

### reverse
```racket
(reverse '(1 2 3 4 5))  ; '(5 4 3 2 1)

; Reverse a string
(list->string (reverse (string->list "hello")))  ; "olleh"
```

### sort
```racket
(sort '(3 1 4 1 5 9 2 6) <)  ; '(1 1 2 3 4 5 6 9)
(sort '(3 1 4 1 5 9 2 6) >)  ; '(9 6 5 4 3 2 1 1)

; Sort strings
(sort '("banana" "apple" "cherry") string<?)
; '("apple" "banana" "cherry")

; Sort by key
(define people '((alice 30) (bob 25) (charlie 35)))
(sort people < #:key cadr)
; '((bob 25) (alice 30) (charlie 35))

; Sort structs
(struct person (name age))
(define persons (list (person "Alice" 30) (person "Bob" 25)))
(sort persons < #:key person-age)
```

### remove-duplicates (unique)
```racket
(remove-duplicates '(1 2 2 3 3 3 4 4 4 4))  ; '(1 2 3 4)

; With custom equality
(remove-duplicates '("Hello" "hello" "HELLO") string-ci=?)
; '("Hello")

; Remove duplicates by key
(define people '((alice 30) (bob 25) (alice 31)))
(remove-duplicates people #:key car)  ; '((alice 30) (bob 25))
```

---

## List Comprehensions (for/list)

```racket
; Basic comprehension
(for/list ([x '(1 2 3 4 5)])
  (* x x))  ; '(1 4 9 16 25)

; With filter (when clause)
(for/list ([x '(1 2 3 4 5 6 7 8 9 10)]
           #:when (even? x))
  (* x x))  ; '(4 16 36 64 100)

; Nested (Cartesian product)
(for/list ([x '(1 2 3)]
           [y '(a b c)])
  (list x y))
; '((1 a) (1 b) (1 c) (2 a) (2 b) (2 c) (3 a) (3 b) (3 c))

; Parallel iteration (like zip)
(for/list ([x '(1 2 3)]
           [y '(10 20 30)])
  (+ x y))  ; '(11 22 33)

; With index
(for/list ([x '(a b c d e)]
           [i (in-naturals)])
  (list i x))  ; '((0 a) (1 b) (2 c) (3 d) (4 e))

; for/fold - fold with comprehension syntax
(for/fold ([sum 0])
          ([x '(1 2 3 4 5)])
  (+ sum x))  ; 15

; for/and and for/or
(for/and ([x '(2 4 6 8)])
  (even? x))  ; #t

(for/or ([x '(1 3 5 7)])
  (even? x))  ; #f
```

---

## Higher-Order Functions

### compose
```racket
; compose - right to left
(define process
  (compose string-upcase
           string-trim
           (λ (s) (string-replace s "_" " "))))

(process "  hello_world  ")  ; "HELLO WORLD"

; compose1 - for single-argument functions only (faster)
(define inc-then-square (compose1 sqr add1))
(inc-then-square 4)  ; 25

; Using ~> for left-to-right (threading)
(require threading)
(~> "  hello_world  "
    (string-replace "_" " ")
    string-trim
    string-upcase)  ; "HELLO WORLD"
```

### curry / curryr
```racket
; curry - partially apply from the left
(define add (λ (a b c) (+ a b c)))
(define add5 (curry add 5))
(define add5-10 ((curry add 5) 10))

(add5 10 20)     ; 35
((add5 10) 20)   ; 35
(add5-10 20)     ; 35

; curryr - partially apply from the right
(define divide (λ (a b) (/ a b)))
(define half (curryr divide 2))
(half 10)  ; 5

; Practical example
(define add-prefix (curry string-append))
(define greet (add-prefix "Hello, "))
(greet "Alice")  ; "Hello, Alice"
```

### apply
```racket
; apply - call function with list as arguments
(apply + '(1 2 3 4 5))  ; 15
(apply max '(3 1 4 1 5 9 2 6))  ; 9
(apply string-append '("hello" " " "world"))  ; "hello world"

; With fixed initial arguments
(apply + 10 20 '(1 2 3))  ; 36 (10 + 20 + 1 + 2 + 3)
```

### andmap / ormap (all / any)
```racket
(define numbers '(2 4 6 8 10))

; andmap - all elements satisfy predicate
(andmap even? numbers)  ; #t
(andmap even? '(2 4 5 8))  ; #f

; ormap - any element satisfies predicate
(ormap odd? numbers)  ; #f
(ormap odd? '(2 4 5 8))  ; #t

; Short-circuit evaluation
(ormap (λ (x) (and (> x 5) x)) '(1 3 7 9))  ; 7 (first match)
```

### argmax / argmin
```racket
(argmax sqr '(-3 1 4 -2))  ; 4 (has largest square: 16)
(argmin sqr '(-3 1 4 -2))  ; 1 (has smallest square: 1)

; Find person with max age
(define people '((alice 30) (bob 25) (charlie 35)))
(argmax cadr people)  ; '(charlie 35)
(argmin cadr people)  ; '(bob 25)
```

### count
```racket
(count even? '(1 2 3 4 5 6 7 8 9 10))  ; 5
(count (λ (x) (> x 5)) '(1 2 3 4 5 6 7 8 9 10))  ; 5
```

---

## Working with Multiple Values

### values and let-values
```racket
; Return multiple values
(define (min-max lst)
  (values (apply min lst) (apply max lst)))

; Receive multiple values
(let-values ([(lo hi) (min-max '(3 1 4 1 5 9 2 6))])
  (printf "Min: ~a, Max: ~a\n" lo hi))

; define-values
(define-values (minimum maximum) (min-max '(3 1 4 1 5 9 2 6)))

; call-with-values
(call-with-values
  (λ () (min-max '(3 1 4 1 5 9 2 6)))
  (λ (lo hi) (- hi lo)))  ; 8 (range)
```

---

## Pattern Matching

```racket
; match - powerful pattern matching
(define (describe x)
  (match x
    [0 "zero"]
    [(? positive?) "positive"]
    [(? negative?) "negative"]))

(describe 5)   ; "positive"
(describe -3)  ; "negative"
(describe 0)   ; "zero"

; Matching lists
(define (sum-list lst)
  (match lst
    ['() 0]
    [(cons head tail) (+ head (sum-list tail))]))

(sum-list '(1 2 3 4 5))  ; 15

; Destructuring
(define (process-point p)
  (match p
    [(list x y) (sqrt (+ (* x x) (* y y)))]
    [(list x y z) (sqrt (+ (* x x) (* y y) (* z z)))]))

(process-point '(3 4))    ; 5.0
(process-point '(1 2 2))  ; 3.0

; Matching structs
(struct point (x y))

(define (point-distance p)
  (match p
    [(point x y) (sqrt (+ (* x x) (* y y)))]))

; With guards
(define (classify-age age)
  (match age
    [(? (λ (a) (< a 13))) 'child]
    [(? (λ (a) (< a 20))) 'teenager]
    [(? (λ (a) (< a 65))) 'adult]
    [_ 'senior]))

; Using quasiquote patterns
(define (parse-expr expr)
  (match expr
    [`(add ,a ,b) (+ a b)]
    [`(mul ,a ,b) (* a b)]
    [`(sub ,a ,b) (- a b)]
    [n n]))

(parse-expr '(add 3 4))  ; 7
(parse-expr '(mul 3 4))  ; 12
```

---

## Sequences and Streams (Lazy Evaluation)

### Sequences
```racket
; Sequences are lazy and work with for loops
(for/list ([x (in-range 5)])
  (* x x))  ; '(0 1 4 9 16)

; in-naturals - infinite sequence
(for/list ([x (in-naturals)]
           [_ (in-range 5)])
  x)  ; '(0 1 2 3 4)

; Sequence operations
(sequence->list (sequence-map sqr (in-range 5)))  ; '(0 1 4 9 16)
(sequence->list (sequence-filter even? (in-range 10)))  ; '(0 2 4 6 8)

; sequence-fold
(sequence-fold + 0 (in-range 1 6))  ; 15
```

### Streams (Lazy Lists)
```racket
; Streams are lazy - elements computed on demand
(define ones (stream-cons 1 ones))  ; Infinite stream of 1s

(stream-ref ones 0)     ; 1
(stream-ref ones 1000)  ; 1

; Fibonacci stream
(define fibs
  (stream-cons 0
    (stream-cons 1
      (stream-map + fibs (stream-rest fibs)))))

(stream->list (stream-take fibs 10))
; '(0 1 1 2 3 5 8 13 21 34)

; Stream operations
(define evens (stream-filter even? (in-naturals)))
(stream->list (stream-take evens 5))  ; '(0 2 4 6 8)

(define squares (stream-map sqr (in-naturals)))
(stream->list (stream-take squares 5))  ; '(0 1 4 9 16)

; for/stream - create streams with comprehension
(define powers-of-2
  (for/stream ([n (in-naturals)])
    (expt 2 n)))

(stream->list (stream-take powers-of-2 10))
; '(1 2 4 8 16 32 64 128 256 512)
```

---

## Hash Tables (Immutable)

```racket
; Create immutable hash
(define h (hash 'a 1 'b 2 'c 3))

; Access
(hash-ref h 'a)  ; 1
(hash-ref h 'x 'default)  ; 'default

; "Update" returns new hash
(define h2 (hash-set h 'd 4))
(hash-ref h2 'd)  ; 4
(hash-has-key? h 'd)  ; #f (original unchanged)

; Remove
(define h3 (hash-remove h 'a))

; Iterate
(hash-map h (λ (k v) (list k (* v 10))))
; '((a 10) (b 20) (c 30))

(for/list ([(k v) (in-hash h)])
  (cons k v))  ; '((a . 1) (b . 2) (c . 3))

; hash-update
(define counts (hash 'a 1 'b 2))
(hash-update counts 'a add1)  ; #hash((a . 2) (b . 2))

; Build hash from list
(for/hash ([pair '((a 1) (b 2) (c 3))])
  (values (car pair) (cadr pair)))
; #hash((a . 1) (b . 2) (c . 3))
```

---

## Practical Examples

### Example 1: Data Processing Pipeline
```racket
(struct employee (name department salary active?) #:transparent)

(define employees
  (list
    (employee "Alice"   "Engineering" 80000 #t)
    (employee "Bob"     "Sales"       60000 #f)
    (employee "Charlie" "Engineering" 90000 #t)
    (employee "Diana"   "Marketing"   55000 #t)
    (employee "Eve"     "Engineering" 75000 #t)))

(define (average lst) (/ (apply + lst) (length lst)))

; 1. Names of active Engineering employees, sorted
(define eng-names
  (~> employees
      (filter employee-active?)
      (filter (λ (e) (string=? (employee-department e) "Engineering")))
      (map employee-name)
      (sort string<?)))

(displayln eng-names)  ; '("Alice" "Charlie" "Eve")

; 2. Average salary of active Engineering employees
(define avg-eng-salary
  (~> employees
      (filter employee-active?)
      (filter (λ (e) (string=? (employee-department e) "Engineering")))
      (map employee-salary)
      average
      exact->inexact))

(printf "~a\n" avg-eng-salary)  ; 81666.666...

; 3. Department headcount and total salary (active only)
(define active-employees (filter employee-active? employees))

(define dept-summary
  (for/fold ([groups (hash)])
            ([e active-employees])
    (hash-update groups
                 (employee-department e)
                 (λ (acc) (list (add1 (car acc))
                                (+ (cadr acc) (employee-salary e))))
                 '(0 0))))

(for ([(dept stats) (in-hash dept-summary)])
  (printf "~a: ~a employees, $~a total\n"
          dept (car stats) (cadr stats)))
; Engineering: 3 employees, $245000 total
; Marketing:   1 employees, $55000 total
```

### Example 2: String Processing
```racket
(define words '("hello" "world" "functional" "programming"))

; Transform pipeline
(define result
  (~> words
      (filter (λ (w) (> (string-length w) 5)))
      (map string-upcase)
      (sort string<?)))

(displayln result)  ; '("FUNCTIONAL" "PROGRAMMING")

; Word frequency
(define text "the quick brown fox jumps over the lazy dog the fox")

(define word-frequency
  (for/fold ([counts (hash)])
            ([word (string-split text)])
    (hash-update counts word add1 0)))

(displayln word-frequency)
; #hash(("the" . 3) ("fox" . 2) ("quick" . 1) ...)

; Top N words
(define top-words
  (~> word-frequency
      hash->list
      (sort > #:key cdr)
      (take 3)))

(displayln top-words)  ; '(("the" . 3) ("fox" . 2) ("quick" . 1))
```

### Example 3: Tree Operations
```racket
; Binary tree
(struct node (value left right) #:transparent)
(struct leaf (value) #:transparent)

; Build a tree
(define tree
  (node 1
        (node 2 (leaf 4) (leaf 5))
        (node 3 (leaf 6) (leaf 7))))

; Map over tree
(define (tree-map f t)
  (match t
    [(leaf v) (leaf (f v))]
    [(node v l r) (node (f v) (tree-map f l) (tree-map f r))]))

(tree-map sqr tree)

; Fold over tree
(define (tree-fold f init t)
  (match t
    [(leaf v) (f init v)]
    [(node v l r) 
     (f (tree-fold f (tree-fold f init l) r) v)]))

(tree-fold + 0 tree)  ; 28 (sum of all values)

; Flatten tree to list
(define (tree->list t)
  (match t
    [(leaf v) (list v)]
    [(node v l r) (append (tree->list l) (list v) (tree->list r))]))

(tree->list tree)  ; '(4 2 5 1 6 3 7) - in-order
```

### Example 4: Parser Combinators
```racket
; Simple expression evaluator using pattern matching
(define (eval-expr expr)
  (match expr
    [(? number?) expr]
    [`(+ ,a ,b) (+ (eval-expr a) (eval-expr b))]
    [`(- ,a ,b) (- (eval-expr a) (eval-expr b))]
    [`(* ,a ,b) (* (eval-expr a) (eval-expr b))]
    [`(/ ,a ,b) (/ (eval-expr a) (eval-expr b))]
    [`(if ,cond ,then ,else)
     (if (eval-expr cond) (eval-expr then) (eval-expr else))]
    [`(> ,a ,b) (> (eval-expr a) (eval-expr b))]
    [`(< ,a ,b) (< (eval-expr a) (eval-expr b))]))

(eval-expr '(+ 1 2))  ; 3
(eval-expr '(* (+ 1 2) (- 5 3)))  ; 6
(eval-expr '(if (> 5 3) 100 200))  ; 100
```

### Example 5: Memoization
```racket
; Manual memoization
(define (memoize f)
  (define cache (make-hash))
  (λ args
    (hash-ref! cache args (λ () (apply f args)))))

; Fibonacci with memoization
(define fib
  (memoize
    (λ (n)
      (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))))

(time (fib 35))  ; Fast due to memoization

; Using define/memo from memoize library
(require memoize)

(define/memo (fib2 n)
  (if (< n 2)
      n
      (+ (fib2 (- n 1)) (fib2 (- n 2)))))
```

### Example 6: Monadic Error Handling
```racket
; Maybe monad-like pattern
(define (safe-div a b)
  (if (zero? b) #f (/ a b)))

(define (safe-sqrt x)
  (if (negative? x) #f (sqrt x)))

; Chain with and
(define (chain-maybe val . fns)
  (for/fold ([result val])
            ([f fns]
             #:break (not result))
    (f result)))

(chain-maybe 16 safe-sqrt (λ (x) (safe-div x 2)))  ; 2
(chain-maybe 16 safe-sqrt (λ (x) (safe-div x 0)))  ; #f
(chain-maybe -16 safe-sqrt (λ (x) (safe-div x 2)))  ; #f

; Using Option-like struct
(struct some (value) #:transparent)
(struct none () #:transparent)

(define (maybe-map f m)
  (match m
    [(some v) (some (f v))]
    [(none) (none)]))

(define (maybe-bind m f)
  (match m
    [(some v) (f v)]
    [(none) (none)]))

(~> (some 16)
    (maybe-bind (λ (x) (if (negative? x) (none) (some (sqrt x)))))
    (maybe-map (λ (x) (* x 2))))  ; (some 8)
```

---

## Useful Functions Reference

```racket
; List operations
cons, car, cdr, list, append, reverse, length
first, second, third, rest, last
take, drop, take-right, drop-right
list-ref, list-set, list-update

; Higher-order
map, filter, foldl, foldr, apply
andmap, ormap, for-each
compose, compose1, curry, curryr

; Predicates
null?, list?, pair?, empty?
member, memf, assoc, assf
index-of, indexes-of

; Transformation
flatten, append-map, remove-duplicates
sort, shuffle, range

; Search
find, findf, argmax, argmin
count, index-of

; Partitioning
partition, splitf-at, split-at
group-by (custom), chunk (from srfi)

; Sequences
in-range, in-naturals, in-list, in-vector
sequence-map, sequence-filter, sequence-fold

; Streams
stream-cons, stream-first, stream-rest
stream-map, stream-filter, stream-take

; Hash tables
hash, hash-ref, hash-set, hash-remove
hash-update, hash-map, hash->list
```

---

## Required Imports

```racket
#lang racket

; Threading macros (for ~> and ~>>)
(require threading)

; Additional list operations
(require racket/list)

; Memoization
(require memoize)

; Additional sequence operations
(require racket/sequence)

; SRFI libraries for more utilities
(require srfi/1)   ; List library
(require srfi/26)  ; cut/cute (partial application)
```

---

## Key Takeaways

1. **Immutable by Default**: Lists, structs, and hash tables are immutable unless explicitly made mutable
2. **First-Class Functions**: Functions can be passed, returned, and stored in data structures
3. **Pattern Matching**: `match` is powerful for destructuring and conditional logic
4. **Lazy Sequences**: Streams and sequences enable efficient processing of large/infinite data
5. **List Comprehensions**: `for/list`, `for/fold`, etc. provide expressive iteration
6. **Tail Recursion**: Racket optimizes tail calls, enabling efficient recursive solutions
7. **Macros**: Racket's macro system allows extending the language for custom abstractions
8. **Multiple Return Values**: `values` and `let-values` handle multiple return values elegantly
