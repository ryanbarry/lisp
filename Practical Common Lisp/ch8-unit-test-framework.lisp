;;;; unit test framework

(defvar *test-name* nil) ; used to hold current test

;; helper macro, CLisp already defines 'with-gensyms' in CL-USER
(defmacro my-with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (my-with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other test
   functions or use 'check' to run individual test cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;;; the following is used to demonstrate usage of the framework from above

;; example of a simple test function
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

;; another
(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

;; this is an example of a test suite, it can use the same deftest macro since
;; that macro keeps track of the depth as we create hierarchy upon hierarchy
(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
