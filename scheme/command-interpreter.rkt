#lang racket
(require "cuban.rkt")

; unary command decorator
; create-unary-command :: (number -> number) -> ([string] -> [string])
(define (create-unary-command f)
    (lambda (stack)
           (cons (number->string (f (string->number (head stack))))
                 (tail stack))))

; binary command decorator
; create-binary-command :: (number -> number) -> ([string] -> [string])
(define (create-binary-command f)
    (lambda (stack)
           (cons (number->string (f (string->number (head (tail stack)))
                                    (string->number (head stack))))
                 (tail (tail stack)))))

; command definitions - command functions have the form:
;   cmd :: [string] -> [string]
(define cmds (hash
  "inv"  (create-unary-command (lambda (a) (/ 1 a)))
  "sqrt" (create-unary-command sqrt)
  "+"    (create-binary-command +)
  "-"    (create-binary-command -)
  "*"    (create-binary-command *)
  "/"    (create-binary-command /)
  "dup"  (lambda (stack) (cons (head stack) stack))
))

; access function associated with op and execute against stack
; process-op :: string -> [string] -> [string]
(define (process-op op stack)
    (cond [(hash-has-key? cmds op) ((hash-ref cmds op) stack)]
          [else (cons op stack)]))  ; op is not command, add to stack

; evaluate ops against stack
; evalute-ops :: [string] -> [string] -> [string]
(define (evaluate-ops ops stack) (foldl process-op stack ops))

; evaluate S-expression
; evaluate-sexp :: string -> null (prints to stdout)
(define (evaluate-sexp s-exp)
    (for-each displayln (reverse (evaluate-ops (string-split s-exp) '()))))


; evaluate command line S-expression
(define (main)
    (define args (current-command-line-arguments))
    (for-each displayln (reverse (evaluate-ops (vector->list args) '()))))

(main)