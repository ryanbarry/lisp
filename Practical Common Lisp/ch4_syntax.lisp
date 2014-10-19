;;;; this is just an example of the different parts of syntax gone over in ch4

;; first, atoms
19 ; an integer
2/3 ; a ratio
1.3 ; a single-precision float
1d3 ; a double-precision float representing 1000.0
"asdf" ; a string
'("a" "list" "of" "strings") ; a list of strings
:id ; a keyword symbol

;; now sexps
(if t (format t "asdf")) ; special form if

(defun hello-world () ; macro usage
    (princ "hello, world"))

(hello-world) ; function usage
