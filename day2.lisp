(defpackage :day-2
  (:use :cl
        :cl-arrows
        :split-sequence)
  (:export :main))

(in-package :day-2)

(defun parse-input (input)
  (->> input
       (split-sequence #\,)
       (mapcar #'parse-integer)))
