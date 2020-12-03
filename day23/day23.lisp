(defpackage #:aoc2019.day23
  (:use #:cl
        #:intcode-interpreter
        #:alexandria))

(in-package #:aoc2019.day23)

(defun initialize-computers (number-of-computers intcode)
  (let ((computers (make-hash-table)))
    (dotimes (i number-of-computers)
      (let ((computer (make-computer intcode)))
        (run-computer computer (list i))
        (setf (gethash i computers) computer)))
    computers))

(defun run-network-part-1 (computers)
  (let ((queue '()))
    (loop for i = 0 then (mod (1+ i) (hash-table-count computers)) do
      (when (and queue (= (car queue) 255))
        (return-from run-network-part-1 (caddr queue)))
      (if (and queue (= (car queue) i))
          (destructuring-bind (address x y . rest) queue
            (declare (ignore address))
            (setf queue rest)
            (appendf queue (run-computer (gethash i computers) (list x y))))
          (appendf queue (run-computer (gethash i computers) (list -1)))))))

(defun run-network-part-2 (computers)
  (loop for i = 0 then (mod (1+ i) (hash-table-count computers))
        with idle-count = 0
        and queues = (make-hash-table)
        and nat = '()
        and last-y-delivered = nil do
          (let ((nat-queue (gethash 255 queues)))
            (when nat-queue
              (setf nat (list (first nat-queue) (second nat-queue))
                    (gethash 255 queues) (cddr nat-queue))))
          (when (>= idle-count 50)
            (when (eql last-y-delivered (second nat))
              (return-from run-network-part-2 last-y-delivered))
            (appendf (gethash 0 queues) nat)
            (setf idle-count 0
                  last-y-delivered (second nat)
                  nat '()))
          (let ((queue (gethash i queues)))
            (if queue
                (destructuring-bind (x y . rest) queue
                  (setf idle-count 0)
                  (setf (gethash i queues) rest)
                  (loop for (address x y) on (run-computer (gethash i computers) (list x y))
                        by #'cdddr do
                          (appendf (gethash address queues) (list x y))))
                (progn
                  (loop for (address x y) on (run-computer (gethash i computers) (list -1))
                        by #'cdddr do
                          (appendf (gethash address queues) (list x y)))
                  (incf idle-count))))))

(defun main (&key (part 2))
  (let ((computers (initialize-computers 50 *intcode*)))
    (case part
      (1 (run-network-part-1 computers))
      (2 (run-network-part-2 computers))
      (otherwise (error "`part' must be either 1 or 2")))))
