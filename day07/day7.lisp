(defpackage :day-7
  (:use :cl
        :cl-arrows
        :split-sequence
        :alexandria)
  (:export :main))

(in-package :day-7)

(declaim (optimize debug))

(defun parse-input (input)
  (->> input
       (split-sequence #\,)
       (map 'vector #'parse-integer)))

(defun get-value (ints current-opcode-index mode-flags argument-index)
  (let ((arg (aref ints (+ current-opcode-index argument-index))))
    (if (eql (nth (1- argument-index) mode-flags) 1)
        arg
        (aref ints arg))))

(defun handle-arithmetic-opcode (f ints current-opcode-index mode-flags)
  (let ((val-1 (get-value ints current-opcode-index mode-flags 1))
        (val-2 (get-value ints current-opcode-index mode-flags 2))
        (output-pos (aref ints (+ current-opcode-index 3))))
    (setf (aref ints output-pos)
          (funcall f val-1 val-2)))
  (+ current-opcode-index 4))

(defun handle-input-opcode (ints current-opcode-index mode-flags)
  (declare (ignore mode-flags))
  (format *error-output* "> ")
  (force-output)
  (let ((pos (aref ints (1+ current-opcode-index)))
        (input (parse-integer (read-line) :junk-allowed t)))
    (setf (aref ints pos) input))
  (+ current-opcode-index 2))

(defun handle-output-opcode (ints current-opcode-index mode-flags)
  (let ((val (get-value ints current-opcode-index mode-flags 1)))
    (print val))
  (+ current-opcode-index 2))

(defun handle-jump-opcode (f ints current-opcode-index mode-flags)
  (let ((val-1 (get-value ints current-opcode-index mode-flags 1))
        (val-2 (get-value ints current-opcode-index mode-flags 2)))
    (if (funcall f val-1 0)
        val-2
        (+ current-opcode-index 3))))

(defun handle-comparison-opcode (f ints current-opcode-index mode-flags)
  (let ((val-1 (get-value ints current-opcode-index mode-flags 1))
        (val-2 (get-value ints current-opcode-index mode-flags 2))
        (output-pos (aref ints (+ current-opcode-index 3))))
    (setf (aref ints output-pos) (if (funcall f val-1 val-2) 1 0)))
  (+ current-opcode-index 4))

(defun process-intcode (ints)
  (let ((new-i 0))
    (loop for i = 0 then new-i
          until (= (aref ints i) 99)
          do (multiple-value-bind (opcode mode-flags)
                 (process-opcode (aref ints i))
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
                         (otherwise (error (format nil "Invalid opcode: ~a" opcode))))))
                 (setf new-i (funcall opcode-func ints i mode-flags)))))))

(defun process-opcode (opcode)
  (values (mod opcode 100)
          (loop for i = (floor (/ opcode 100)) then (floor (/ i 10))
                while (> i 0)
                collecting (mod i 10))))

(defun calculate-output-signal (ints phase-settings)
  (loop for phase-setting in phase-settings
        for s = (make-array '(0) :element-type 'base-char
                                 :fill-pointer 0 :adjustable t)
        with input-signal = 0
        do (with-input-from-string
               (*standard-input* (format nil "~d~%~d~%" phase-setting input-signal))
             (with-output-to-string (*standard-output* s)
               (process-intcode (copy-seq ints))
               (setf input-signal (parse-integer s :junk-allowed t))))
        finally (return input-signal)))

(defun calculate-maximum-output-signal (ints)
  (apply #'max
         (mapcar (curry #'calculate-output-signal ints)
                 (all-permutations '(0 1 2 3 4)))))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
                 append (mapcar (lambda (l) (cons element l))
                                (all-permutations (remove element list)))))))

(defun main ()
  (let* ((ints (with-open-file (input "input.txt")
                 (-> input
                     (read-line nil)
                     parse-input))))
    (calculate-maximum-output-signal ints)))
