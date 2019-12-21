(defpackage #:day19
  (:use #:cl #:intcode-interpreter #:alexandria))

(in-package :day19)

(defun check-area (computer area-size)
  (let ((area (make-hash-table :test 'equal)))
    (loop for y from 0 below area-size do
      (loop for x from 0 below area-size do
        (setf (gethash (cons x y) area)
              (car (run-computer computer (list x y))))
        (reset-computer computer)))
    area))

(defun main (&key (part 1))
  (let* ((computer (make-computer (parse-input (read-file-into-string "input.txt"))))
         (area (check-area computer 50)))
    (case part
      (1 (count-if (curry #'eql 1) (hash-table-values area)))
      (2 (error "unimplemented"))
      (otherwise (error "`part' must be either 1 or 2")))))
