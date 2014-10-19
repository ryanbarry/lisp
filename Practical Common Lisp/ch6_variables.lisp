;;;; variables in lisp are fancy

;; make a closure
(defvar *weird* (let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count))))

;; all three functions are accessing the same variable binding
(funcall (first *weird*)) ; increment

(funcall (second *weird*)) ; decrement

(funcall (third *weird*)) ; get current value
