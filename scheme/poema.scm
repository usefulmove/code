#lang racket

(define (day-to-night hours)
  (cond [(<= hours 6) (greet-dawn)]
        [(<= hours 12) (enjoy-morning)]
        [(<= hours 18) (embrace-afternoon)]
        [(<= hours 20) (witness-dusk)]
        [else (embrace-midnight)]))

(define (greet-dawn)
  (displayln "Light paints the sky, unveiling the day"))

(define (enjoy-morning)
  (displayln "Sun ascends high, life's vibrant play"))

(define (embrace-afternoon)
  (displayln "Light turns warm, time slowly weaves"))

(define (witness-dusk)
  (displayln "Sun bows down, shadows grow long"))

(define (embrace-midnight)
  (displayln "Night enfolds, under the stars we belong"))

(day-to-night 5)  ; try various inputs to see the "poem" unfold
