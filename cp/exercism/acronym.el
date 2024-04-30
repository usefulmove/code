;;; acronym.el --- Acronym (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun acronym (phrase)
  (let ((words (string-split phrase "[ \f\t\n\r\v\-]+"))
        (char-alpha-p (lambda (c)
                        (let ((c (downcase c)))
                          (<= ?a c ?z)))))
    (mapconcat
     (lambda (word)
       (letrec ((recurse (lambda (lst)
                           (cond ((null lst) "")
                                 ((funcall char-alpha-p (car lst))
                                  (upcase (string (car lst))))
                                 (t (funcall recurse (cdr lst)))))))
         (funcall recurse (string-to-list word))))
     words)))


(provide 'acronym)
;;; acronym.el ends here
