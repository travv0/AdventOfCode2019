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

(defun handle-opcode (f ints current-opcode-index)
  (let ((pos-1 (aref ints (+ current-opcode-index 1)))
        (pos-2 (aref ints (+ current-opcode-index 2)))
        (result-pos (aref ints (+ current-opcode-index 3))))
    (setf (aref ints result-pos)
          (funcall f (aref ints pos-1) (aref ints pos-2)))))

(defun process-intcode (ints)
  (loop for i = 0 then (+ i 4)
        until (= (aref ints i) 99)
        do (let ((opcode (aref ints i)))
             (cond ((= opcode 1) (handle-opcode #'+ ints i))
                   ((= opcode 2) (handle-opcode #'* ints i))
                   (t (error (format nil "Invalid opcode: ~a" opcode))))))
  ints)

(defun restore-state (ints)
  (setf (aref ints 1) 12
        (aref ints 2) 2)
  ints)

(defun main (&key (part 1))
  (let* ((input (open "day2.txt"))
         (ints (-> input
                   (read-line nil)
                   parse-input
                   restore-state)))
    (aref (cond ((= part 1) (process-intcode ints))
                ((= part 2) (error "unimplemented"))
                (t (error "`part' must be either 1 or 2")))
          0)))
