#lang racket

(require racket/gui/easy)

(render
 (window
  (hpanel
   (button "-" (λ () (printf "button - clicked\n")))
   (text "( dcode )")
   (button "+" (λ () (printf "button + clicked\n"))))))