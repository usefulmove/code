(define (script-fu-split-tone image drawable)
  (let* ((width (car (gimp-image-get-width image)))
         (height (car (gimp-image-get-height image)))
         (current-layer drawable))
    ; apply color balance
    (gimp-drawable-color-balance current-layer
                                 TRANSFER-SHADOWS
                                 FALSE
                                 0.0 0.0 1.5)
    (gimp-drawable-color-balance current-layer
                                 TRANSFER-MIDTONES
                                 FALSE
                                 2.0 1.5 0.0)
    (gimp-drawable-color-balance current-layer
                                 TRANSFER-HIGHLIGHTS
                                 FALSE
                                 4.0 3.0 0.0)

    ; update all open displays to show changes
    (gimp-displays-flush)))

(script-fu-register
 "script-fu-split-tone"                         ; Procedure name
 "Split Tone (Selenium-Sepia)"                  ; Menu label
 "Adds split tone effect to current layer."     ; Description
 "Duane Edmonds"                                ; Author
 "Duane Edmonds"                                ; Copyright
 "2025"                                         ; Date
 "*"                                            ; Image types (all)
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
)

(script-fu-menu-register "script-fu-split-tone"
                         "<Image>/Filters/Illuminated Film")
