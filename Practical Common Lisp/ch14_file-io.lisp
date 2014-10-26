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

(let ((file (open "./ch14_testfile.txt")))
  (format t "~%") ; start with a newline, to separate from any previous output
  (loop for c = (read-char file nil)
       while c do (format t "~a'" c))
  (print '(EOF)) ; denote where the file actually ends in output, helpful when
  (close file))  ; a file ends with a newline or some other non-printing char
