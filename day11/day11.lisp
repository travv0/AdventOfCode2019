(defpackage :day11
  (:use #:cl
        #:intcode-interpreter
        #:alexandria))

(in-package :day11)

(declaim (optimize safety))

(defconstant +directions+ '(:up :right :down :left))

(defun direction-p (thing)
  (some (curry #'eql thing) +directions+))

(deftype direction ()
  `(satisfies direction-p))

(defclass robot ()
  ((direction :initarg :direction
              :initform :up
              :accessor direction
              :type direction)
   (position :initarg :position
             :initform (cons 0 0)
             :accessor robot-position)))

(defmethod rotate-clockwise ((robot robot))
  (setf (direction robot) (nth (mod (1+ (or (position (direction robot)
                                                      +directions+)
                                            0))
                                    (length +directions+))
                               +directions+)))

(defmethod rotate-counterclockwise ((robot robot))
  (let ((direction-count (length +directions+)))
    (setf (direction robot) (nth (mod (1- (+ (or (position (direction robot)
                                                           +directions+)
                                                 0)
                                             direction-count))
                                      direction-count)
                                 +directions+))))

(defmethod move ((robot robot))
  (with-slots ((dir direction) (pos position))
      robot
    (case dir
      (:up (decf (cdr pos)))
      (:down (incf (cdr pos)))
      (:right (incf (car pos)))
      (:left (decf (car pos))))
    pos))

(defun draw-panels (panels)
  (let* ((keys (hash-table-keys panels))
         (xs (mapcar #'car keys))
         (ys (mapcar #'cdr keys))
         (min-x (apply #'min xs))
         (max-x (apply #'max xs))
         (min-y (apply #'min ys))
         (max-y (apply #'max ys)))
    (loop for y from min-y to max-y do
      (loop for x from min-x to max-x do
        (format t "~a" (case (gethash (cons x y) panels 0)
                         (0 " ")
                         (1 "."))))
      (format t "~%"))))

(defun main (&key (part 2))
  (let* ((intcode (parse-input (read-file-into-string "input.txt")))
         (computer (make-computer intcode))
         (robot (make-instance 'robot))
         (panels (make-hash-table :test 'equal)))
    (when (= part 2)
      (setf (gethash (cons 0 0) panels) 1))
    (loop
      (multiple-value-bind (output complete)
          (run-computer computer (list (gethash (robot-position robot) panels 0)))
        (let ((color-code (first output))
              (rotation-code (second output))
              (position (robot-position robot)))
          (setf (gethash (copy-list position) panels) color-code)
          (case rotation-code
            (0 (rotate-counterclockwise robot))
            (1 (rotate-clockwise robot)))
          (move robot)
          (when complete
            (return (case part
                      (1 (hash-table-count panels))
                      (2 (draw-panels panels))))))))))
