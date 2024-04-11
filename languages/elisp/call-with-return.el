(defmacro call-with-return (tag &rest body)
  "Define a block that can be exited with call-return."
  `(catch ,tag
     ,@body))

(defmacro call-return (tag value)
  "Exit the nearest enclosing call-with-return block, returning VALUE."
  `(throw ,tag ,value))

(call-with-return 'exit-point
 (dolist (x '(1 2 3))
   (if (= x 2)
       (call-return 'exit-point x)))  ; And here as well.
 (message "This line is only reached if the throw is not triggered."))
