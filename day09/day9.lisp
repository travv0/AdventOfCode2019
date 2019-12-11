(defpackage :day9
  (:use :cl
        :cl-arrows
        :split-sequence
        :alexandria)
  (:export :parse-input
           :process-intcode))

(in-package :day9)

(defvar %*original-incode* nil)
(defparameter *intcode* nil)
(defparameter *relative-base* 0)
(defparameter *position* 0)
(defparameter *process-complete* nil)

(defun reset-computer ()
  (setf *intcode* nil
        *relative-base* 0
        *position* 0
        *process-complete* nil))

(defun parse-input (input)
  (let* ((digit-strings (split-sequence #\, input)))
    (make-array (length digit-strings)
                :initial-contents (mapcar #'parse-integer digit-strings)
                :adjustable t)))

(defun access-ints (index)
  (when (<= (length *intcode*) index)
    (setf *intcode* (adjust-array *intcode* (+ index 1))))
  (aref *intcode* index))

(defun (setf access-ints) (new-value ints index)
  (when (<= (length *intcode*) index)
    (adjust-array *intcode* (+ index 1)))
  (setf (aref *intcode* index) new-value))

(defun get-value (mode-flags argument-index)
  (let ((arg (access-ints (+ *position* argument-index)))
        (code (nth (1- argument-index) mode-flags)))
    (case code
      ((nil 0) (access-ints arg))
      (1 arg)
      (2 (access-ints (+ *relative-base* arg)))
      (otherwise (error (format nil "Invalid mode code: ~a" code))))))

(defun get-output-pos (mode-flags argument-index)
  (let ((arg (access-ints (+ *position* argument-index)))
        (code (nth (1- argument-index) mode-flags)))
    (case code
      ((nil 0) arg)
      (1 (error "Immediate mode not valid for output"))
      (2 (+ *relative-base* arg))
      (otherwise (error (format nil "Invalid mode code: ~a" code))))))

(defun handle-arithmetic-opcode (f mode-flags)
  (let ((val-1 (get-value mode-flags 1))
        (val-2 (get-value mode-flags 2))
        (output-pos (get-output-pos mode-flags 3)))
    (setf (access-ints output-pos)
          (funcall f val-1 val-2)))
  (+ *position* 4))

(defun handle-input-opcode (mode-flags)
  (format t "> ")
  (force-output)
  (let ((pos (get-output-pos mode-flags 1))
        (input (parse-integer (read-line) :junk-allowed t)))
    (setf (access-ints pos) input))
  (+ *position* 2))

(defun handle-output-opcode (mode-flags)
  (let ((val (get-value mode-flags 1)))
    (format t "~d~%" val))
  (+ *position* 2))

(defun handle-jump-opcode (f mode-flags)
  (let ((val-1 (get-value mode-flags 1))
        (val-2 (get-value mode-flags 2)))
    (if (funcall f val-1 0)
        val-2
        (+ *position* 3))))

(defun handle-comparison-opcode (f mode-flags)
  (let ((val-1 (get-value mode-flags 1))
        (val-2 (get-value mode-flags 2))
        (output-pos (get-output-pos mode-flags 3)))
    (setf (access-ints output-pos) (if (funcall f val-1 val-2) 1 0)))
  (+ *position* 4))

(defun handle-relative-base-opcode (mode-flags)
  (let ((val (get-value mode-flags 1)))
    (incf *relative-base* val))
  (+ *position* 2))

(defun process-intcode ()
  (when (null *intcode*)
    (setf *intcode* %*original-incode*))
  (loop until (= (access-ints *position*) 99)
        finally (setf *process-complete* t)
        do (multiple-value-bind (opcode mode-flags)
               (process-opcode (access-ints *position*))
             (let ((opcode-func
                     (case opcode
                       (1 (curry #'handle-arithmetic-opcode #'+))
                       (2 (curry #'handle-arithmetic-opcode #'*))
                       (3 #'handle-input-opcode)
                       (4 #'handle-output-opcode)
                       (5 (curry #'handle-jump-opcode #'/=))
                       (6 (curry #'handle-jump-opcode #'=))
                       (7 (curry #'handle-comparison-opcode #'<))
                       (8 (curry #'handle-comparison-opcode #'=))
                       (9 #'handle-relative-base-opcode)
                       (otherwise (error (format nil "Invalid opcode: ~a" opcode))))))
               (setf *position* (funcall opcode-func mode-flags))))))

(defun process-opcode (opcode)
  (values (mod opcode 100)
          (loop for i = (floor (/ opcode 100)) then (floor (/ i 10))
                while (> i 0)
                collecting (mod i 10))))

(defun main ()
  (let* ((%*original-incode* (with-open-file (input "input.txt")
                               (-> input
                                   (read-line nil)
                                   parse-input)))
         (*intcode* %*original-incode*)
         (*relative-base* 0)
         (*position* 0)
         (*process-complete* nil))
    (process-intcode)))
