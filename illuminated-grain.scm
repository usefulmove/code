(define (script-fu-add-grain-400tx image drawable)
  (let* ((width (car (gimp-image-get-width image)))
         (height (car (gimp-image-get-height image)))
         (layer (car (gimp-layer-new image
                                     "Grain (400TX)"
                                     width
                                     height
                                     RGB-IMAGE 
                                     100.0
                                     LAYER-MODE-GRAIN-MERGE))))
    (gimp-image-insert-layer image layer 0 -1)
    (gimp-context-set-foreground '(128 128 128))
    (gimp-drawable-fill layer FILL-FOREGROUND)
    ; update all open displays to show changes
    (gimp-displays-flush)))

(script-fu-register
 "script-fu-add-grain-400tx"                    ; Procedure name
 "Add Grain (400TX)"                            ; Menu label
 "Adds a Tri-X 400 (400TX) film grain emulation layer"     ; Description
 "Duane Edmonds"                                ; Author
 "Duane Edmonds"                                ; Copyright
 "2025"                                         ; Date
 "*"                                            ; Image types (all)
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
)

(script-fu-menu-register "script-fu-add-grain-400tx"
                         "<Image>/Filters/Illuminated Film Grain")
