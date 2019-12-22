(defpackage #:day19
  (:use #:cl #:intcode-interpreter #:alexandria))

(in-package :day19)

(defun check-area (computer area-size)
  (let ((area (make-hash-table :test 'equal)))
    (loop for y from 0 below area-size do
      (loop for x from 0 below area-size do
        (setf (gethash (cons x y) area)
              (car (run-computer computer (list x y))))
        (reset-computer computer)))
    area))

(defun check-square (area square-pos square-size)
  (destructuring-bind (square-x . square-y) square-pos
    (loop for y from square-y below (+ square-y square-size) do
      (loop for x from square-x below (+ square-x square-size)
            when (eql (gethash (cons x y) area) 0) do
              (return-from check-square))
          finally (return-from check-square t))))

(defun find-square-pos (computer square-size)
  (let ((area (make-hash-table :test 'equal)))
    (loop for y = 0 then (1+ y)
          for seen-beam = nil
          with last-seen-beam-pos = 0 do
            (loop for x = 0 then (1+ x) do
              (let ((result (car (run-computer computer (list x y)))))
                (when (and (not seen-beam)
                           (= result 1))
                  (setf seen-beam t
                        last-seen-beam-pos x))
                (setf (gethash (cons x y) area) result)
                (when (and (>= x square-size)
                           (>= y square-size)
                           (check-square area
                                         (cons (- x square-size)
                                               (- y square-size))
                                         square-size))
                  (return-from find-square-pos (cons (- x square-size)
                                                     (- y square-size))))
                (reset-computer computer)
                (when (and (or seen-beam
                               (> x (* (1+ last-seen-beam-pos) 10)))
                           (= result 0))
                  (return)))))))

(defun main (&key (part 2))
  (let* ((computer (make-computer (parse-input (read-file-into-string "input.txt")))))
    (case part
      (1 (count-if (curry #'eql 1)
                   (hash-table-values (check-area computer 50))))
      (2 (destructuring-bind (x . y) (find-square-pos computer 100)
           (+ (* x 10000) y)))
      (otherwise (error "`part' must be either 1 or 2")))))
