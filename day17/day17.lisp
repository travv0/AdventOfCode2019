(defpackage :day17
  (:use #:cl
        #:alexandria
        #:intcode-interpreter))

(in-package :day17)

(defun convert-computer-output (input)
  (let ((lines (str:lines (map 'string #'code-char input))))
    (make-array (list (length lines) (length (car lines)))
                :initial-contents lines)))

(defun calculate-alignment-parameters (scaffolding)
  (loop for y from 0 below (array-dimension scaffolding 0) do
    (loop for x from 0 below (second (array-dimensions scaffolding))
          when (is-intersection scaffolding x y)
            sum (* x y))))

(defun is-intersection (scaffolding x y)
  (when (and (> x 0)
             (> y 0)
             (< x (- (second (array-dimensions scaffolding)) 3))
             (< y (- (array-dimension scaffolding 0) 2)))
    (every (lambda (c) (char= c #\#))
           (list (aref scaffolding (1- x) y)
                 (aref scaffolding (1+ x) y)
                 (aref scaffolding x (1- y))
                 (aref scaffolding x (1+ y))))))
