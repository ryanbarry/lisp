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

(with-open-file (file "./ch14_testfile.txt")
  (fresh-line) ; always start on a newline; won't add one if already there
  (loop for line = (read-line file nil)
       while line do (format t "~a~%" line))
  (princ "[EOF]"))

(with-open-file (file "./ch14_testfile.txt")
  (read-line file nil)
  (fresh-line)
  (let ((2nd-line (file-position file)))
    (file-position file (+ 2nd-line 2))
    (princ (read-line file nil))))
