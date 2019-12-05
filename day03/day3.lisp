(defpackage :day-3
  (:use :cl))

(in-package :day-3)

(defun up (grid wire-num curr-x curr-y count)
  (loop for y from curr-y to (+ curr-y count) do
    (let* ((coords (format nil "~d,~d" curr-x y))
           (wire-table (gethash coords grid)))
      (if wire-table
          (setf (gethash wire-num wire-table) t)
          (let ((new-wire-table (make-hash-table)))
            (setf (gethash wire-num new-wire-table) t)
            (setf (gethash coords grid) new-wire-table)))))
  grid)
