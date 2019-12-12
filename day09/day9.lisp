(defpackage :intcode-interpreter
  (:use #:cl
        #:cl-arrows
        #:split-sequence
        #:alexandria)
  (:export #:parse-input
           #:complete-p
           #:make-computer
           #:run-computer
           #:reset-computer))

(in-package :intcode-interpreter)

(declaim (optimize safety))

(defclass computer ()
  ((%original-intcode :initarg :intcode
                      :initform (error "original-intcode is required")
                      :reader original-intcode)
   (intcode :accessor intcode)
   (relative-base :initform 0 :accessor relative-base)
   (position :initform 0 :accessor current-position)
   (inputs :initform '() :accessor inputs)
   (outputs :initform '() :accessor outputs)))

(defmethod reset-intcode ((com computer))
  (setf (intcode com) (make-array (array-dimensions (original-intcode com))
                                  :initial-contents (original-intcode com)
                                  :adjustable t)))

(defmethod initialize-instance :after ((com computer) &key)
  (reset-intcode com))

(defun make-computer (intcode)
  (make-instance 'computer :intcode intcode))

(defmethod reset-computer ((com computer))
  (reset-intcode com)
  (setf (relative-base com) 0
        (current-position com) 0
        (inputs com) '()
        (outputs com) '()))

(defun parse-input (input)
  (let* ((digit-strings (split-sequence #\, input)))
    (make-array (length digit-strings)
                :initial-contents (mapcar #'parse-integer digit-strings)
                :adjustable t)))

(defmethod access-ints ((com computer) index)
  (when (<= (length (intcode com)) index)
    (setf (intcode com) (adjust-array (intcode com) (+ index 1))))
  (aref (intcode com) index))

(defmethod (setf access-ints) (new-value (com computer) index)
  (when (<= (length (intcode com)) index)
    (adjust-array (intcode com) (+ index 1)))
  (setf (aref (intcode com) index) new-value))

(defmethod get-value ((com computer) mode-flags argument-index)
  (let ((arg (access-ints com (+ (current-position com) argument-index)))
        (code (nth (1- argument-index) mode-flags)))
    (case code
      ((nil 0) (access-ints com arg))
      (1 arg)
      (2 (access-ints com (+ (relative-base com) arg)))
      (otherwise (error (format nil "Invalid mode code: ~a" code))))))

(defmethod get-output-pos ((com computer) mode-flags argument-index)
  (let ((arg (access-ints com (+ (current-position com) argument-index)))
        (code (nth (1- argument-index) mode-flags)))
    (case code
      ((nil 0) arg)
      (1 (error "Immediate mode not valid for output"))
      (2 (+ (relative-base com) arg))
      (otherwise (error (format nil "Invalid mode code: ~a" code))))))

(defmethod handle-arithmetic-opcode (f (com computer) mode-flags)
  (let ((val-1 (get-value com mode-flags 1))
        (val-2 (get-value com mode-flags 2))
        (output-pos (get-output-pos com mode-flags 3)))
    (setf (access-ints com output-pos)
          (funcall f val-1 val-2)))
  (+ (current-position com) 4))

(defmethod get-input ((com computer))
  (pop (inputs com)))

(defmethod flush-outputs ((com computer))
  (let ((outputs (outputs com)))
    (setf (outputs com) '())
    outputs))

(defmethod handle-input-opcode ((com computer) mode-flags)
  (let ((pos (get-output-pos com mode-flags 1))
        (input (get-input com)))
    (setf (access-ints com pos) input))
  (+ (current-position com) 2))

(defmethod handle-output-opcode ((com computer) mode-flags)
  (let ((val (get-value com mode-flags 1)))
    (setf (outputs com) (append (outputs com) (list val))))
  (+ (current-position com) 2))

(defmethod handle-jump-opcode (f (com computer) mode-flags)
  (let ((val-1 (get-value com mode-flags 1))
        (val-2 (get-value com mode-flags 2)))
    (if (funcall f val-1 0)
        val-2
        (+ (current-position com) 3))))

(defmethod handle-comparison-opcode (f (com computer) mode-flags)
  (let ((val-1 (get-value com mode-flags 1))
        (val-2 (get-value com mode-flags 2))
        (output-pos (get-output-pos com mode-flags 3)))
    (setf (access-ints com output-pos) (if (funcall f val-1 val-2) 1 0)))
  (+ (current-position com) 4))

(defmethod handle-relative-base-opcode ((com computer) mode-flags)
  (let ((val (get-value com mode-flags 1)))
    (incf (relative-base com) val))
  (+ (current-position com) 2))

(defmethod complete-p ((com computer))
  (and (intcode com) (= (access-ints com (current-position com)) 99)))

(defmethod run-computer ((com computer) &optional inputs)
  (setf (inputs com) (append (inputs com) inputs))
  (loop until (complete-p com)
        finally (return-from run-computer (values (flush-outputs com) t))
        do (multiple-value-bind (opcode mode-flags)
               (process-opcode (access-ints com (current-position com)))
             (let ((opcode-func
                     (case opcode
                       (1 (curry #'handle-arithmetic-opcode #'+))
                       (2 (curry #'handle-arithmetic-opcode #'*))
                       (3 (if (inputs com)
                              #'handle-input-opcode
                              (return-from run-computer
                                (values (flush-outputs com) nil))))
                       (4 #'handle-output-opcode)
                       (5 (curry #'handle-jump-opcode #'/=))
                       (6 (curry #'handle-jump-opcode #'=))
                       (7 (curry #'handle-comparison-opcode #'<))
                       (8 (curry #'handle-comparison-opcode #'=))
                       (9 #'handle-relative-base-opcode)
                       (otherwise (error (format nil "Invalid opcode: ~a" opcode))))))
               (setf (current-position com) (funcall opcode-func com mode-flags))))))

(defun process-opcode (opcode)
  (values (mod opcode 100)
          (loop for i = (floor (/ opcode 100)) then (floor (/ i 10))
                while (> i 0)
                collecting (mod i 10))))

(defun main (inputs)
  (let ((computer (make-computer (with-open-file (input "input.txt")
                                   (-> input
                                       (read-line nil)
                                       parse-input)))))
    (run-computer computer inputs)))
