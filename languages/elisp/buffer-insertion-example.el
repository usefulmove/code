(defun test ()
  (switch-to-buffer "(dcode)")
  (insert "this is what it is")
  (let ((bold-text (propertize " and it do what it do." 'face 'bold)))
    (insert bold-text))) ; test

(test)
