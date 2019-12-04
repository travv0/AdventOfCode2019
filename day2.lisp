(defpackage :day-2
  (:use :cl
        :cl-arrows
        :split-sequence)
  (:export :main))

(in-package :day-2)

(defun parse-input (input)
  (->> input
       (split-sequence #\,)
       (map 'vector #'parse-integer)))

(defun add-opcode (ints current-opcode-index)
  (let ((pos-1 (aref ints (+ current-opcode-index 1)))
        (pos-2 (aref ints (+ current-opcode-index 2))))

    )
  )
