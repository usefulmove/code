(define (script-fu-portra image drawable)
  (let* ((width (car (gimp-image-get-width image)))
         (height (car (gimp-image-get-height image)))
         (current-layer drawable))
    ;; start undo group
    (gimp-image-undo-group-start image)

    ;; apply color balance
    (gimp-drawable-color-balance current-layer
                                 TRANSFER-SHADOWS
                                 FALSE
                                 0.0 0.0 3.0)
    (gimp-drawable-color-balance current-layer
                                 TRANSFER-MIDTONES
                                 FALSE
                                 6.0 4.5 0.0)
    (gimp-drawable-color-balance current-layer
                                 TRANSFER-HIGHLIGHTS
                                 FALSE
                                 12.0 9.0 0.0)

    ;; end undo group
    (gimp-image-undo-group-end image)

    ;; update all open displays to show changes
    (gimp-displays-flush)))

(script-fu-register
 "script-fu-portra"                             ; Procedure name
 "Portra Film Emulation"                        ; Menu label
 "Adds warm tone to image layer"                ; Description
 "Duane Edmonds"                                ; Author
 "Duane Edmonds"                                ; Copyright
 "2025"                                         ; Date
 "*"                                            ; Image types (all)
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 )

(script-fu-menu-register "script-fu-portra"
                         "<Image>/Filters/Illuminated Film")
