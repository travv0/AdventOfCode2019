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
  (aref ints 0))

(defun set-state (ints noun verb)
  (setf (aref ints 1) noun
        (aref ints 2) verb)
  ints)

(defun find-noun-and-verb (ints expected-output)
  (loop for potential-noun from 0 to 99 do
    (loop for potential-verb from 0 to 99 do
      (let ((initial-ints (copy-seq ints)))
        (set-state initial-ints potential-noun potential-verb)
        (when (= (process-intcode initial-ints) expected-output)
          (return-from find-noun-and-verb (+ (* 100 potential-noun) potential-verb)))))))

(defun main (&key (part 2))
  (let* ((ints (with-open-file (input "input.txt")
                 (-> input
                     (read-line nil)
                     parse-input
                     (set-state 12 2)))))
    (cond ((= part 1) (process-intcode ints))
          ((= part 2) (find-noun-and-verb ints 19690720))
          (t (error "`part' must be either 1 or 2")))))
