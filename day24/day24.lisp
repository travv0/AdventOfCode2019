(defpackage #:day24
  (:use #:cl #:alexandria))

(in-package #:day24)

(defun parse-input (input)
  (let ((planet 0))
    (loop for line in (str:lines input) for y = 0 then (1+ y) do
      (loop for char across line for x = 0 then (1+ x) do
        (when (char= char #\#)
          (setf planet (dpb 1 (get-byte x y) planet)))))
    planet))

(defun simulate-minute (planet)
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

(defun get-byte (x y)
  (let ((index (+ x (* y 5))))
    (if (<= 0 index 24)
        (byte 1 index)
        (byte 0 0))))

(defun print-planet (planet)
  (loop for c across (reverse (format nil "~b" planet))
        with i = 0
        do (format t "~c" (if (char= c #\1) #\# #\.))
           (incf i)
           (when (>= i 5)
             (format t "~%")
             (setf i 0))))

(defun simulate-until-match (planet)
  (loop with past-states = (list planet)
        with new-state = (simulate-minute planet)
        until (position new-state past-states)
        finally (return new-state)
        do (push new-state past-states)
           (setf new-state (simulate-minute new-state))))

(defun main (&key (part 1))
  (let ((planet (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (simulate-until-match planet))
      (2 (error "unimplemented"))
      (otherwise (error "`part' must be either 1 or 2")))))
