(defpackage :day17
  (:use #:cl
        #:alexandria
        #:intcode-interpreter))

(in-package :day17)

(defun convert-computer-output (output)
  (let ((lines (str:lines (map 'string #'code-char output) :omit-nulls t)))
    (make-array (list (length lines) (length (car lines)))
                :initial-contents lines)))

(defun calculate-alignment-parameters (scaffolding)
  (loop for y from 0 below (array-dimension scaffolding 0)
        sum (loop for x from 0 below (second (array-dimensions scaffolding))
                  when (is-intersection scaffolding x y)
                    sum (* x y))))

(defun is-intersection (scaffolding x y)
  (when (and (> x 0)
             (> y 0)
             (< x (1- (second (array-dimensions scaffolding))))
             (< y (1- (array-dimension scaffolding 0))))
    (every (lambda (c) (char= c #\#))
           (list (aref scaffolding y x)
                 (aref scaffolding (1- y) x)
                 (aref scaffolding (1+ y) x)
                 (aref scaffolding y (1- x))
                 (aref scaffolding y (1+ x))))))

(defun main (&key (part 1))
  (let* ((computer (make-computer
                    (parse-input (read-file-into-string "input.txt"))))
         (scaffolding (convert-computer-output (run-computer computer))))
    (case part
      (1 (calculate-alignment-parameters scaffolding))
      (2 (error "unimplemented"))
      (otherwise (error "`part' must be either 1 or 2")))))
