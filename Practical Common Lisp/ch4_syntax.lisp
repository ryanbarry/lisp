;;;; this is just an example of the different parts of syntax gone over in ch4

;; first, atoms
19 ; a number
"asdf" ; a string
'("a" "list" "of" "strings") ; a list of strings

;; now sexps
(if t (format t "asdf")) ; special form if

(defun hello-world () ; macro usage
    (princ "hello, world"))

(hello-world) ; function usage
