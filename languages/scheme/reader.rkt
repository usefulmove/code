#lang racket
(require 2htdp/image)
(require 2htdp/universe)

; create an image with text
(define text-image
  (text "now is the time" 52 "black"))

; display the image in a window
(define (main)
  (big-bang 0
            (on-draw (lambda (state) text-image))
            (stop-when (lambda (state) #f))))

(main)
