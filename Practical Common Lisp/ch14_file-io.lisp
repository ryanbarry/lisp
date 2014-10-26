(let ((file (open "./ch14_testfile.txt")))
  (format t "~a~%" (read-line file))
  (close file))
