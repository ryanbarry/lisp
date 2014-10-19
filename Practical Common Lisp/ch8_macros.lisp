;;;; CH 8 is where i learn how to build my own macros

;; example 1: do-primes
(defun primep (number)
  "helper function #1"
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "helper function #2"
  (loop for n from number when (primep n) return n))

;;; example of desired macro form
;;
;; (doprimes (p 0 19)
;;   (format t "~d" p))
;;
;; this should execute the body once for each prime number >= 0 and <= 19 with
;; the variable p holding the prime number

;;; example of desired macro expansion
;;
;; (do ((p (next-prime 0) (next-prime (1+ p))))
;;     ((> p 19))
;;   (format t "~d" p))
;;
;; this would be how we'd implement the functionality using do

;; first attempt
(defmacro do-primes1 (var-and-range &rest body)
  (let ((var (first var-and-range))
	(start (second var-and-range))
	(end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
	 ((> ,var ,end))
       ,@body)))

;; second, this time using destructuring
(defmacro do-primes2 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ p))))
       ((> ,var ,end))
     ,@body))

;; third, fix abstraction leak: 'end is evaluated >1 times
(defmacro do-primes3 ((var start end) &body body)
  `(do ((ending-value ,end)
	(,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

;; fourth, fix abstraction leak: 'start & 'end are evaluated in opposite
;; order compared to how they're passed in
(defmacro do-primes4 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	(ending-value ,end))
       ((> ,var ending-value))
     ,@body))

;; final, fix abstraction leak: use gensym to make names for use inside macro
(defmacro do-primes5 ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))


;;; macros can write macros too!

;;; example macro form
;;
;; (defmacro do-primes ((var start end) &body body)
;;   (with-gensyms (ending-value-name)
;;     `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
;; 	  (,ending-value-name ,end))
;; 	 ((> ,var ,ending-value-name))
;;        ,@body)))
;;
;; this should produce equivalent code to do-primes5

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
