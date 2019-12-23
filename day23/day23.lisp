(defpackage #:day23
  (:use #:cl
        #:intcode-interpreter
        #:alexandria))

(in-package #:day23)

(defun initialize-computers (number-of-computers intcode)
  (let ((computers (make-hash-table)))
    (dotimes (i number-of-computers)
      (let ((computer (make-computer intcode)))
        (run-computer computer (list i))
        (setf (gethash i computers) computer)))
    computers))

(defun run-network (computers)
  (let ((queue '()))
    (loop for i = 0 then (mod (1+ i) (hash-table-count computers)) do
      (when (and queue (= (car queue) 255))
        (return-from run-network (caddr queue)))
      (if (and queue (= (car queue) i))
          (destructuring-bind (address x y . rest) queue
            (declare (ignore address))
            (setf queue rest)
            (appendf queue (run-computer (gethash i computers) (list x y))))
          (appendf queue (run-computer (gethash i computers) (list -1)))))))

(defun main (&key (part 1))
  (case part
    (1 (run-network (initialize-computers 50 *intcode*)))
    (2 (error "unimplemented"))
    (otherwise (error "`part' must be either 1 or 2"))))
