(defpackage :aoc2019.day7
  (:use :cl
        :cl-arrows
        :split-sequence
        :alexandria)
  (:export :main))

(in-package :aoc2019.day7)

(defclass amplifier ()
  ((intcode :initarg :intcode :reader intcode)
   (position :initform 0)
   (inputs :initarg :inputs :initform '())
   (complete :reader complete-p :initform nil)))

(defmethod push-input ((amp amplifier) input)
  (let ((inputs (slot-value amp 'inputs)))
    (setf (slot-value amp 'inputs) (append inputs (list input)))))

(defmethod pop-input ((amp amplifier))
  (let* ((inputs (slot-value amp 'inputs))
         (input (car inputs)))
    (setf (slot-value amp 'inputs) (cdr inputs))
    input))

(defmethod mark-complete ((amp amplifier))
  (setf (slot-value amp 'complete) t))

(defun parse-input (input)
  (->> input
       (split-sequence #\,)
       (map 'vector #'parse-integer)))

(defun get-value (ints current-opcode-index mode-flags argument-index)
  (let ((arg (aref ints (+ current-opcode-index argument-index))))
    (if (eql (nth (1- argument-index) mode-flags) 1)
        arg
        (aref ints arg))))

(defmethod handle-arithmetic-opcode (f (amp amplifier) mode-flags)
  (let* ((ints (intcode amp))
         (current-opcode-index (slot-value amp 'position))
         (val-1 (get-value ints current-opcode-index mode-flags 1))
         (val-2 (get-value ints current-opcode-index mode-flags 2))
         (output-pos (aref ints (+ current-opcode-index 3))))
    (setf (aref ints output-pos)
          (funcall f val-1 val-2))
    (incf (slot-value amp 'position) 4)))

(defmethod handle-input-opcode ((amp amplifier) mode-flags)
  (declare (ignore mode-flags))
  (let* ((ints (intcode amp))
         (current-opcode-index (slot-value amp 'position))
         (pos (aref ints (1+ current-opcode-index)))
         (input (pop-input amp)))
    (setf (aref ints pos) input)
    (incf (slot-value amp 'position) 2)))

(defmethod handle-output-opcode ((amp amplifier) mode-flags)
  (let* ((current-opcode-index (slot-value amp 'position))
         (val (get-value (intcode amp) current-opcode-index mode-flags 1)))
    (print val)
    (incf (slot-value amp 'position) 2)))

(defmethod handle-jump-opcode (f (amp amplifier) mode-flags)
  (let* ((ints (intcode amp))
         (current-opcode-index (slot-value amp 'position))
         (val-1 (get-value ints current-opcode-index mode-flags 1))
         (val-2 (get-value ints current-opcode-index mode-flags 2)))
    (if (funcall f val-1 0)
        (setf (slot-value amp 'position) val-2)
        (incf (slot-value amp 'position) 3))))

(defmethod handle-comparison-opcode (f (amp amplifier) mode-flags)
  (let* ((ints (intcode amp))
         (current-opcode-index (slot-value amp 'position))
         (val-1 (get-value ints current-opcode-index mode-flags 1))
         (val-2 (get-value ints current-opcode-index mode-flags 2))
         (output-pos (aref ints (+ current-opcode-index 3))))
    (setf (aref ints output-pos) (if (funcall f val-1 val-2) 1 0))
    (incf (slot-value amp 'position) 4)))

(defmethod process-intcode ((amp amplifier))
  (let ((ints (intcode amp)))
    (loop for i = (slot-value amp 'position)
          until (= (aref ints i) 99)
          do (multiple-value-bind (opcode mode-flags)
                 (process-opcode (aref ints i))
               (let ((opcode-func
                       (case opcode
                         (1 (curry #'handle-arithmetic-opcode #'+))
                         (2 (curry #'handle-arithmetic-opcode #'*))
                         (3 (if (slot-value amp 'inputs) #'handle-input-opcode (return-from process-intcode)))
                         (4 #'handle-output-opcode)
                         (5 (curry #'handle-jump-opcode #'/=))
                         (6 (curry #'handle-jump-opcode #'=))
                         (7 (curry #'handle-comparison-opcode #'<))
                         (8 (curry #'handle-comparison-opcode #'=))
                         (otherwise (error (format nil "Invalid opcode: ~a" opcode))))))
                 (funcall opcode-func amp mode-flags)))
          finally (mark-complete amp))))

(defun process-opcode (opcode)
  (values (mod opcode 100)
          (loop for i = (floor (/ opcode 100)) then (floor (/ i 10))
                while (> i 0)
                collecting (mod i 10))))

(defun calculate-output-signal (ints phase-settings)
  (let ((amplifiers (loop for i from 0 to 4 collecting (make-instance 'amplifier :intcode (copy-seq ints))))
        (input-signal 0)
        (complete-in-a-row 0))
    (mapc (lambda (amp ps) (push-input amp ps)) amplifiers phase-settings)
    (loop for amp-num = 0 then (mod (1+ amp-num) 5)
          for s = (make-array '(0) :element-type 'base-char
                                   :fill-pointer 0 :adjustable t)
          do (let ((amp (nth amp-num amplifiers)))
               (if (complete-p amp)
                   (incf complete-in-a-row)
                   (with-output-to-string (*standard-output* s)
                     (setf complete-in-a-row 0)
                     (push-input amp input-signal)
                     (process-intcode amp)
                     (setf input-signal (parse-integer s :junk-allowed t))))
               (when (>= complete-in-a-row 5)
                 (return-from calculate-output-signal input-signal))))))

(defun calculate-maximum-output-signal (ints phase-settings)
  (apply #'max
         (mapcar (curry #'calculate-output-signal ints)
                 (all-permutations phase-settings))))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
                 append (mapcar (lambda (l) (cons element l))
                                (all-permutations (remove element list)))))))

(defun main (&key (part 2))
  (let* ((ints (with-open-file (input "input.txt")
                 (-> input
                     (read-line nil)
                     parse-input))))
    (calculate-maximum-output-signal ints (case part
                                            (1 '(0 1 2 3 4))
                                            (2 '(5 6 7 8 9))
                                            (otherwise (error "`part' must be either 1 or 2"))))))
