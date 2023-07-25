#lang racket
(require racket/contract)
(require "dcode.scm")

; unary command decorator
; create-unary-command :: (number -> number) -> ([string] -> [string])
(define/contract (create-unary-command f)
    (-> (-> number? number?) (-> (listof string?) (listof string?)))
    (lambda (stack)
           (cons (number->string (f (string->number (head stack))))
                 (tail stack))))

; binary command decorator
; create-binary-command :: (number -> number -> number) -> ([string] -> [string])
(define/contract (create-binary-command f)
    (-> (-> number? number? number?) (-> (listof string?) (listof string?)))
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
  "x"    (create-binary-command *)
  "/"    (create-binary-command /)
  "dup"  (lambda (stack) (cons (head stack) stack))
))

; access function associated with op and execute against stack
; process-op :: string -> [string] -> [string]
(define/contract (process-op op stack)
    (-> string? (listof string?) (listof string?))
    (cond [(hash-has-key? cmds op) ((hash-ref cmds op) stack)]
          [else (cons op stack)]))  ; op is not command, add to stack

; evaluate ops against stack
; evalute-ops :: [string] -> [string] -> [string]
(define/contract (evaluate-ops ops stack)
    (-> (listof string?) (listof string?) (listof string?))
    (foldl process-op stack ops))

; evaluate S-expression
; evaluate-sexp :: string -> null (prints to stdout)
(define/contract (evaluate-sexp s-exp)
    (-> string? null)
    (for-each displayln (reverse (evaluate-ops (string-split s-exp) '()))))


; evaluate command line S-expression
(define (main)
    (define args (current-command-line-arguments))
    (for-each displayln (reverse (evaluate-ops (vector->list args) '()))))

(main)