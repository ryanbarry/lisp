(let ((file (open "./ch14_testfile.txt")))
  (format t "~a~%" (read-line file))
  (close file))

(let ((file (open "./ch14_testfile.txt" :if-does-not-exist nil)))
  (when file
    (format t "~a~%" (read-line file))
    (close file)))

(let ((file (open "./ch14_testfile.txt" :if-does-not-exist nil)))
  (when file
    (loop for line = (read-line file nil)
	 while line do (format t "~a~%" line))
    (print '(EOF))
    (close file)))
