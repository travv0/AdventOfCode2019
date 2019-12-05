(defpackage :day-3
  (:use :cl))

(in-package :day-3)

(defun lay-wire (grid wire-num curr-x curr-y axis arith-func count)
  (let ((changing-coord (if (eq axis :x) curr-x curr-y)))
    (loop for i = (funcall arith-func
                           changing-coord
                           1)
            then (funcall arith-func i 1)
          until (= i (funcall arith-func
                              (funcall arith-func changing-coord 1)
                              count))
          finally (return-from lay-wire (values grid curr-x curr-y))
          do (let* ((coords (if (eq axis :x)
                                (format nil "~d,~d" i curr-y)
                                (format nil "~d,~d" curr-x i)))
                    (wire-table (gethash coords grid)))
               (if wire-table
                   (setf (gethash wire-num wire-table) t)
                   (let ((new-wire-table (make-hash-table)))
                     (setf (gethash wire-num new-wire-table) t)
                     (setf (gethash coords grid) new-wire-table)))))))

(defun up (grid wire-num curr-x curr-y count)
  (lay-wire grid wire-num curr-x curr-y :y #'- count))

(defun down (grid wire-num curr-x curr-y count)
  (lay-wire grid wire-num curr-x curr-y :y #'+ count))

(defun left (grid wire-num curr-x curr-y count)
  (lay-wire grid wire-num curr-x curr-y :x #'- count))

(defun right (grid wire-num curr-x curr-y count)
  (lay-wire grid wire-num curr-x curr-y :x #'+ count))
