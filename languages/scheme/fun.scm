#lang racket

(define ooo (lambda (ooo oo) (ooo oo oo oo)))

(ooo + 1) ; 3 - triple
(ooo - 1) ; -1 - negate
(ooo * 1) ; 1 - cube
(ooo / 1) ; 1 - invert

(ooo + 2) ; 6
(ooo - 2) ; -2
(ooo * 2) ; 8
(ooo / 2) ; 1/2

(ooo + 8) ; 24
(ooo - 8) ; -8
(ooo * 8) ; 512
(ooo / 8) ; 1/8

(ooo + -1) ; -3
(ooo - -1) ; 1
(ooo * -1) ; -1
(ooo / -1) ; -1

(define oo (lambda (oo ooo) (oo ooo ooo)))

(oo + 1) ; 2 - double
(oo - 1) ; 0 - zero
(oo * 1) ; 1 - square
(oo / 1) ; 1 - one

(oo + 2) ; 4
(oo - 2) ; 0
(oo * 2) ; 4
(oo / 2) ; 1

(oo + 8) ; 16
(oo - 8) ; 0
(oo * 8) ; 64
(oo / 8) ; 1

(oo + -1) ; -2
(oo - -1) ; 0
(oo * -1) ; 1
(oo / -1) ; 1

(oo cons 2) ; (2 . 2)

(oo expt 1) ; 1
(oo expt 2) ; 4
(oo expt 4) ; 256
(oo expt 8) ; 16777216


(ooo + (oo + 1)) ; 6
(ooo + (oo + 2)) ; 12
(ooo + (oo + 8)) ; 48
(ooo * (oo * 1)) ; 1
(ooo * (oo * 2)) ; 64
(ooo * (oo * 8)) ; 262144

(map (lambda (x) (ooo * x)) (range 1 (add1 10))) ; (1 8 27 64 125 216 343 512 729 1000)


(oo expt 6) ; 46656
(oo + (oo expt 4)) ; 512
