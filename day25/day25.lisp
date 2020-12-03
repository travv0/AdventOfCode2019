(defpackage #:aoc2019.day25
  (:use #:cl #:alexandria #:intcode-interpreter))

(in-package #:aoc2019.day25)

(defun main ()
  (let ((computer (make-computer (parse-input (read-file-into-string "input.txt")))))
    (format t "~a~%" (map 'string #'code-char (run-computer computer)))
    (loop
      (format t "> ")
      (multiple-value-bind (output done)
          (run-computer computer (append (map 'list #'char-code (read-line)) '(10)))
        (format t "~a~%" (map 'string #'code-char output))
        (when done
          (return))))))
