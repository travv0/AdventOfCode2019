(defpackage #:aoc2019.day21
  (:use #:cl #:alexandria #:intcode-interpreter))

(in-package #:aoc2019.day21)

(defparameter *springscript-walk* (format nil "~
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
"))

(defparameter *springscript-run* (format nil "~
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
AND H T
OR H T
OR E T
AND T J
RUN
"))

(defun main (&key (part 2))
  (let* ((computer (make-computer (parse-input (read-file-into-string "input.txt"))))
         (result (run-computer computer (map 'list #'char-code
                                             (case part
                                               (1 *springscript-walk*)
                                               (2 *springscript-run*)
                                               (otherwise "`part' must be either 1 or 2")))))
         (last-digit (car (last result))))
    (if (> last-digit 255)
        (format t "~a~%" last-digit)
        (format t "~a~%" (map 'string #'code-char result)))))
