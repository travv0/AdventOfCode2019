(defpackage :day-5
  (:use :cl
        :cl-arrows
        :split-sequence)
  (:export :main))

(in-package :day-5)

(defun parse-input (input)
  (->> input
       (split-sequence #\,)
       (map 'vector #'parse-integer)))

(defun handle-arithmetic-opcode (f ints current-opcode-index)
  (let ((pos-1 (aref ints (+ current-opcode-index 1)))
        (pos-2 (aref ints (+ current-opcode-index 2)))
        (result-pos (aref ints (+ current-opcode-index 3))))
    (setf (aref ints result-pos)
          (funcall f (aref ints pos-1) (aref ints pos-2))))
  4)

(defun handle-input-opcode (ints current-opcode-index)
  (format t "> ")
  (force-output)
  (let ((pos (aref ints (1+ current-opcode-index)))
        (input (parse-integer (read-line) :junk-allowed t)))
    (setf (aref ints pos) input))
  2)

(defun handle-output-opcode (ints current-opcode-index)
  (let ((pos (aref ints (1+ current-opcode-index))))
    (print (aref ints pos)))
  2)

(defun process-intcode (ints)
  (let ((inc 0))
    (loop for i = 0 then (+ i inc)
          until (= (aref ints i) 99)
          do (let ((opcode (process-opcode (aref ints i))))
               (setf inc
                     (case opcode
                       (1 (handle-arithmetic-opcode #'+ ints i))
                       (2 (handle-arithmetic-opcode #'* ints i))
                       (3 (handle-input-opcode ints i))
                       (4 (handle-output-opcode ints i))
                       (otherwise (error (format nil "Invalid opcode: ~a" opcode))))))))
  (aref ints 0))

(defun process-opcode (opcode)
  (values (mod opcode 100)
          (loop for i = (floor (/ opcode 100)) then (floor (/ i 10))
                for j = 0 then (incf j)
                while (< j 3)
                collecting (mod i 10))))

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
                     parse-input))))
    (cond ((= part 1) (process-intcode ints))
          ((= part 2) (find-noun-and-verb ints 19690720))
          (t (error "`part' must be either 1 or 2")))))
