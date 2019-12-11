(defpackage :day9
  (:use :cl
        :cl-arrows
        :split-sequence
        :alexandria)
  (:export :main))

(in-package :day9)

(defvar *relative-base*)

(defun parse-input (input)
  (let* ((digit-strings (split-sequence #\, input)))
    (make-array (length digit-strings)
                :initial-contents (mapcar #'parse-integer digit-strings)
                :adjustable t)))

(defun access-ints (ints index)
  (when (<= (length ints) index)
    (setf ints (adjust-array ints (+ index 1))))
  (aref ints index))

(defun (setf access-ints) (new-value ints index)
  (when (<= (length ints) index)
    (adjust-array ints (+ index 1)))
  (setf (aref ints index) new-value))

(defun get-value (ints current-opcode-index mode-flags argument-index)
  (let ((arg (access-ints ints (+ current-opcode-index argument-index)))
        (code (nth (1- argument-index) mode-flags)))
    (case code
      ((nil 0) (access-ints ints arg))
      (1 arg)
      (2 (access-ints ints (+ *relative-base* arg)))
      (otherwise (error (format nil "Invalid mode code: ~a" code))))))

(defun get-output-pos (ints current-opcode-index mode-flags argument-index)
  (let ((arg (access-ints ints (+ current-opcode-index argument-index)))
        (code (nth (1- argument-index) mode-flags)))
    (case code
      ((nil 0) arg)
      (1 (error "Immediate mode not valid for output"))
      (2 (+ *relative-base* arg))
      (otherwise (error (format nil "Invalid mode code: ~a" code))))))

(defun handle-arithmetic-opcode (f ints current-opcode-index mode-flags)
  (let ((val-1 (get-value ints current-opcode-index mode-flags 1))
        (val-2 (get-value ints current-opcode-index mode-flags 2))
        (output-pos (get-output-pos ints current-opcode-index mode-flags 3)))
    (setf (access-ints ints output-pos)
          (funcall f val-1 val-2)))
  (+ current-opcode-index 4))

(defun handle-input-opcode (ints current-opcode-index mode-flags)
  (format t "> ")
  (force-output)
  (let ((pos (get-output-pos ints current-opcode-index mode-flags 1))
        (input (parse-integer (read-line) :junk-allowed t)))
    (setf (access-ints ints pos) input))
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
        (output-pos (get-output-pos ints current-opcode-index mode-flags 3)))
    (setf (access-ints ints output-pos) (if (funcall f val-1 val-2) 1 0)))
  (+ current-opcode-index 4))

(defun handle-relative-base-opcode (ints current-opcode-index mode-flags)
  (let ((val (get-value ints current-opcode-index mode-flags 1)))
    (incf *relative-base* val))
  (+ current-opcode-index 2))

(defun process-intcode (ints)
  (let ((new-i 0))
    (loop for i = 0 then new-i
          until (= (access-ints ints i) 99)
          do (multiple-value-bind (opcode mode-flags)
                 (process-opcode (access-ints ints i))
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
                 (setf new-i (funcall opcode-func ints i mode-flags)))))))

(defun process-opcode (opcode)
  (values (mod opcode 100)
          (loop for i = (floor (/ opcode 100)) then (floor (/ i 10))
                while (> i 0)
                collecting (mod i 10))))

(defun main ()
  (let* ((ints (with-open-file (input "input.txt")
                 (-> input
                     (read-line nil)
                     parse-input)))
         (*relative-base* 0))
    (process-intcode ints)))
