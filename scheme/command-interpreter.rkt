#lang racket

; unary command decorator
; create-unary-command :: (number -> number) -> ([string] -> [string])
(define (create-unary-command f)
    (lambda (stack)
           (cons (number->string (f (string->number (car stack))))
                 (cdr stack))))

; binary command decorator
; create-binary-command :: (number -> number) -> ([string] -> [string])
(define (create-binary-command f)
    (lambda (stack)
           (cons (number->string (f (string->number (car (cdr stack)))
                                    (string->number (car stack))))
                 (cdr (cdr stack)))))

; command definitions - command functions have the form:
;   cmd :: [string] -> [string]
(define cmds (hash
  "+"    (create-binary-command +)
  "-"    (create-binary-command -)
  "*"    (create-binary-command *)
  "/"    (create-binary-command /)
  "dup"  (lambda (stack) (cons (car stack) stack))
  "inv"  (create-unary-command (lambda (a) (/ 1 a)))
  "sqrt" (create-unary-command sqrt)
))

; access function associated with op and execute against stack
; process-op :: string -> [string] -> [string]
(define (process-op op stack)
    (cond
        [(hash-has-key? cmds op) ((hash-ref cmds op) stack)]
        [else (cons op stack)]))  ; op is not command, add to stack

; evaluate ops against stack
; evalute-ops :: [string] -> [string] -> [string]
(define (evaluate-ops ops stack) (foldl process-op stack ops))


; evaluate command line S-expression
(define (main)
    (define args (current-command-line-arguments))
    (define s-expression (vector->list args))
    (for-each displayln (reverse (evaluate-ops s-expression '()))))

(main)