#lang racket

(require racket/contract)
(require "dcode.scm")


; unary command decorator
; unary-command :: (number -> number) -> ([string] -> [string])
(define/contract (unary-command f)
  (-> (-> number? number?) (-> (listof string?) (listof string?)))
  (lambda (stack) (cons (number->string (f (string->number (first stack))))
                        (rest stack))))

; binary command decorator
; binary-command :: (number -> number -> number) -> ([string] -> [string])
(define/contract (binary-command f)
  (-> (-> number? number? number?) (-> (listof string?) (listof string?)))
  (lambda (stack) (cons (number->string (f (string->number (second stack))
                                           (string->number (first stack))))
                        (rest (rest stack)))))

; command-swap :: [string] -> [string]
(define command-swap (lambda (stack) (let ([a (first stack)]
                                           [b (second stack)]
                                           [rst (drop 2 stack)])
                                       (append (list b a) rst))))

; command-iota :: [string] -> [string]
(define command-iota
  (lambda (stack) (let ([a (first stack)]
                        [rst (drop 1 stack)])
                    (append
                      (map number->string (range 1 (add1 (string->number a))))
                      rst))))

; command definitions - command functions must have the form:
;   cmd :: [string] -> [string]
(define cmds (hash
  "inv"  (unary-command (lambda (a) (/ a)))
  "sqrt" (unary-command sqrt)
  "+"    (binary-command +)
  "-"    (binary-command -)
  "*"    (binary-command *)
  "x"    (binary-command *)  ; helpful on command line ("*" has to be escaped)
  "/"    (binary-command /)
  "^"    (binary-command expt)
  "dup"  (lambda (stack) (cons (first stack) stack))
  "iota" command-iota
  "swap" command-swap
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
  (let ([args (current-command-line-arguments)])
    (for-each displayln (reverse (evaluate-ops (vector->list args) '())))))

(main)
