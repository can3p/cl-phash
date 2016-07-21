(in-package #:cl-phash/benchmark)

(defun next-input (stream)
  (let ((l (read-line stream nil)))
    (if l
        (let ((chars (split-sequence #\Space l)))
          (cons (car chars)
                (mapcar #'parse-integer (cdr chars))))
        nil)))

(defun run-from-stream (in out)
  (let ((state (make-array 10 :adjustable t :fill-pointer 0)))
    (vector-push (empty-map) state)
    (loop for command = (next-input in)
          while command
          do (format out "~a~%" (process-command state command)))))

(defun process-command (state command)
  (let ((last-hash (vector-pop state)))
    (vector-push last-hash state)
    (cond
      ((string= (car command) "=")
       (vector-push last-hash state)
       (if (equal (m-get (if (equal 4 (length command)) (aref state (cadddr command)) last-hash) (cadr command)) (caddr command)) 1 0))
      ((string= (car command) "+")
       (let ((new-hash (m-set last-hash (cadr command) (caddr command))))
         (vector-push new-hash state)
         (m-count new-hash)))
      ((string= (car command) "-")
       (let ((new-hash (m-del last-hash (cadr command))))
         (vector-push new-hash state)
         (m-count new-hash))))))

(defun process-and-print-from-file (fname)
  (with-open-file (in fname :direction :input)
    (let ((out (make-string-output-stream))) (run-from-stream in out)
      (format t "~a" (get-output-stream-string out)))))

(defun process-and-compare (in-fname out-fname)
  (with-open-file (in in-fname :direction :input)
    (with-open-file (test-in out-fname :direction :input)
      (let ((out (make-string-output-stream))
            (data (make-string (file-length test-in))))
        (read-sequence data test-in)
        (run-from-stream in out)
        (string= (get-output-stream-string out)
                 data
                 )))))
