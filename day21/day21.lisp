(defpackage #:day21
  (:use #:cl #:alexandria #:intcode-interpreter))

(in-package #:day21)

(defparameter *springscript* (format nil "~
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
"))

(defun main ()
  (let* ((computer (make-computer (parse-input (read-file-into-string "input.txt"))))
         (result (run-computer computer (map 'list #'char-code *springscript*)))
         (last-digit (car (last result))))
    (if (> last-digit 255)
        (format t "~a~%" last-digit)
        (format t "~a~%" (map 'string #'code-char result)))))
