(defpackage :day-6
  (:use :cl
        :alexandria
        :split-sequence
        :cl-graph
        :cl-containers))

(in-package :day-6)

(defun count-orbits-for-planet (planet-vertex)
  (let ((children (child-vertexes planet-vertex)))
    (if children
        (1+ (reduce #'+ (mapcar #'count-orbits-for-planet children)))
        0)))

(defun count-total-orbits (orbits)
  (reduce #'+ (mapcar #'count-orbits-for-planet (vertexes orbits))))

(defun input-to-orbits (lines)
  (let ((orbits (make-container 'graph-container :default-edge-type :directed)))
    (loop for (inner outer) in (mapcar (curry #'split-sequence #\)) lines)
          do (add-edge-between-vertexes orbits (intern outer) (intern inner)))
    orbits))

(defun main ()
  (let ((lines (with-open-file (input "input.txt")
                 (loop for line = (read-line input nil)
                       while line
                       collect line))))
    (count-total-orbits (input-to-orbits lines))))
