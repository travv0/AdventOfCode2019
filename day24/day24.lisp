(defpackage #:aoc2019.day24
  (:use #:cl #:alexandria))

(in-package #:aoc2019.day24)

(defun parse-input (input)
  (let ((planet 0))
    (loop for line in (str:lines input) for y = 0 then (1+ y) do
      (loop for char across line for x = 0 then (1+ x) do
        (when (char= char #\#)
          (setf planet (dpb 1 (get-byte x y) planet)))))
    planet))

(defun simulate-minute-part-1 (planet)
  (let ((new-planet 0))
    (loop for y from 0 below 5 do
      (loop for x from 0 below 5 do
        (setf new-planet
              (cond ((and (ldb-test (get-byte x y) planet)
                          (/= 1 (count 1
                                       (list (when (< x 4) (ldb (get-byte (1+ x) y) planet))
                                             (when (> x 0) (ldb (get-byte (1- x) y) planet))
                                             (ldb (get-byte x (1+ y)) planet)
                                             (ldb (get-byte x (1- y)) planet)))))
                     (dpb 0 (get-byte x y) new-planet))
                    ((and (not (ldb-test (get-byte x y) planet))
                          (position (count 1
                                           (list (when (< x 4) (ldb (get-byte (1+ x) y) planet))
                                                 (when (> x 0) (ldb (get-byte (1- x) y) planet))
                                                 (ldb (get-byte x (1+ y)) planet)
                                                 (ldb (get-byte x (1- y)) planet)))
                                    '(1 2)))
                     (dpb 1 (get-byte x y) new-planet))
                    (t (dpb (ldb (get-byte x y) planet)
                            (get-byte x y)
                            new-planet))))))
    new-planet))

(defun simulate-minute-part-2 (planet-levels)
  (let ((new-planet-levels (make-hash-table)))
    (let* ((level-nums (hash-table-keys planet-levels))
           (start-level (1- (apply #'min level-nums)))
           (end-level (1+ (apply #'max level-nums))))
      (loop for level from start-level to end-level
            for planet = (gethash level planet-levels 0) do
              (loop for y from 0 below 5 do
                (loop for x from 0 below 5
                      for new-planet = (gethash level new-planet-levels 0)
                      unless (and (= x 2) (= y 2)) do
                        (setf (gethash level new-planet-levels)
                              (cond ((and (ldb-test (get-byte x y) planet)
                                          (/= 1 (count 1 (get-surrounding-cells x y level planet-levels))))
                                     (dpb 0 (get-byte x y) new-planet))
                                    ((and (not (ldb-test (get-byte x y) planet))
                                          (position (count 1 (get-surrounding-cells x y level planet-levels))
                                                    '(1 2)))
                                     (dpb 1 (get-byte x y) new-planet))
                                    (t (dpb (ldb (get-byte x y) planet)
                                            (get-byte x y)
                                            new-planet))))))))
    new-planet-levels))

(defun get-surrounding-cells (x y level planet-levels)
  (let ((planet (gethash level planet-levels 0))
        (inner-planet (gethash (1+ level) planet-levels 0))
        (outer-planet (gethash (1- level) planet-levels 0)))
    (flatten
     (list
      ;; right
      (cond ((and (= x 1) (= y 2))
             (mapcar (lambda (y) (ldb (get-byte 0 y) inner-planet))
                     (iota 5)))
            ((= x 4) (ldb (get-byte 3 2) outer-planet))
            (t (ldb (get-byte (1+ x) y) planet)))

      ;; left
      (cond ((and (= x 3) (= y 2))
             (mapcar (lambda (y) (ldb (get-byte 4 y) inner-planet))
                     (iota 5)))
            ((= x 0) (ldb (get-byte 1 2) outer-planet))
            (t (ldb (get-byte (1- x) y) planet)))

      ;; down
      (cond ((and (= x 2) (= y 1))
             (mapcar (lambda (x) (ldb (get-byte x 0) inner-planet))
                     (iota 5)))
            ((= y 4) (ldb (get-byte 2 3) outer-planet))
            (t (ldb (get-byte x (1+ y)) planet)))

      ;; up
      (cond ((and (= x 2) (= y 3))
             (mapcar (lambda (x) (ldb (get-byte x 4) inner-planet))
                     (iota 5)))
            ((= y 0) (ldb (get-byte 2 1) outer-planet))
            (t (ldb (get-byte x (1- y)) planet)))))))

(defun get-byte (x y)
  (let ((index (+ x (* y 5))))
    (if (<= 0 index 24)
        (byte 1 index)
        (byte 0 0))))

(defun print-planet (planet)
  (loop for c across (reverse (format nil "~25b" planet))
        with i = 0
        do (format t "~c" (if (char= c #\1) #\# #\.))
           (incf i)
           (when (>= i 5)
             (format t "~%")
             (setf i 0)))
  (format t "~%"))

(defun simulate-until-match (planet)
  (loop with past-states = (list planet)
        with new-state = (simulate-minute-part-1 planet)
        until (position new-state past-states)
        finally (return new-state)
        do (push new-state past-states)
           (setf new-state (simulate-minute-part-1 new-state))))

(defun main (&key (part 2))
  (let ((planet (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (simulate-until-match planet))
      (2 (let ((planet-levels (make-hash-table)))
           (setf (gethash 0 planet-levels) planet)
           (loop repeat 200 do
             (setf planet-levels (simulate-minute-part-2 planet-levels)))
           (reduce #'+ (mapcar #'logcount (hash-table-values planet-levels)))))
      (otherwise (error "`part' must be either 1 or 2")))))
