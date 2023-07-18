#lang racket

; unary command decorator
; build-unary-command :: (number -> number) -> ([string] -> [string])
(define (build-unary-command f)
    (lambda (stack)
           (cons (number->string (f (string->number (car stack))))
                 (cdr stack))))

; binary command decorator
; build-binary-command :: (number -> number) -> ([string] -> [string])
(define (build-binary-command f)
    (lambda (stack)
           (cons (number->string (f (string->number (car (cdr stack)))
                                    (string->number (car stack))))
                 (cdr (cdr stack)))))

; command definitions
; command functions have the form:
;   cmd :: [string] -> [string]
(define cmds (hash
  "+"    (build-binary-command +)
  "-"    (build-binary-command -)
  "*"    (build-binary-command *)
  "/"    (build-binary-command /)
  "dup"  (lambda (stack) (cons (car stack) stack))
  "inv"  (build-unary-command (lambda (a) (/ 1 a)))
  "sqrt" (build-unary-command sqrt)
))

; access function associated with op and execute against stack
; process-op :: string -> [string] -> [string]
(define (process-op op stack)
    (cond
        [(hash-has-key? cmds op) ((hash-ref cmds op) stack)]
        [else (cons op stack)]))  ; op is not command, add to stack

; evaluate ops against stack
; evalute-ops :: [string] -> [string] -> [string]
(define (evaluate-ops ops stack)
    (foldl process-op stack ops))

; validation
(evaluate-ops (string-split "5 sqrt 1 - 2 / dup inv") '())
