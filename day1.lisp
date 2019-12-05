(defpackage :day-1
  (:use :cl)
  (:export :main))

(in-package :day-1)

(defun calculate-fuel (mass)
  (- (floor (/ mass 3))
     2))

(defun calculate-total-fuel (mass)
  (let ((fuel (calculate-fuel mass)))
    (if (> fuel 0)
        (+ fuel (calculate-total-fuel fuel))
        0)))

(defun sum-fuels (fuel-calc-func masses)
  (reduce #'+ (mapcar fuel-calc-func masses)))

(defun main (&key (part 2))
  (let* ((input (open "day1.txt"))
         (masses (loop for line = (read-line input nil)
                       while line
                       collect (parse-integer line))))
    (cond ((= part 1) (sum-fuels #'calculate-fuel masses))
          ((= part 2) (sum-fuels #'calculate-total-fuel masses))
          (t (format t "`part' must be either 1 or 2")))))
