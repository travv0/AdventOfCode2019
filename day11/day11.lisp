(defpackage :day11
  (:use #:cl
        #:intcode-interpreter
        #:alexandria))

(in-package :day11)

(defconstant +directions+ '(:north :east :south :west))

(defun direction-p (thing)
  (some (curry #'eql thing) +directions+))

(deftype direction ()
  `(satisfies direction-p))

(defclass robot ()
  ((direction :initarg :direction
              :initform :nort
              :accessor direction
              :type direction)))
