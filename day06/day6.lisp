(defpackage :day-6
  (:use :cl
        :alexandria
        :split-sequence
        :cl-graph
        :cl-containers))

(in-package :day-6)

(defun list-orbiting-planet-vertexes (planet-vertex)
  (let ((child (first (child-vertexes planet-vertex))))
    (when child
        (cons child (list-orbiting-planet-vertexes child)))))

(defun count-total-orbits (orbits)
  (reduce #'+ (mapcar (compose #'length #'list-orbiting-planet-vertexes) (vertexes orbits))))

(defun input-to-orbits (lines)
  (let ((orbits (make-container 'graph-container :default-edge-type :directed)))
    (loop for (inner outer) in (mapcar (curry #'split-sequence #\)) lines)
          do (add-edge-between-vertexes orbits (intern outer) (intern inner)))
    orbits))

(defun find-number-of-orbital-transfers (orbits start end)
  (let ((vertexes-from-start (list-orbiting-planet-vertexes (find-vertex orbits start)))
        (vertexes-from-end (list-orbiting-planet-vertexes (find-vertex orbits end))))
    (loop for vertex in vertexes-from-start
          for i = 0 then (1+ i)
          for pos = (position vertex vertexes-from-end)
          when pos
            return (+ pos i))))

(defun main (&key (part 2))
  (let* ((lines (with-open-file (input "input.txt")
                 (loop for line = (read-line input nil)
                       while line
                       collect line)))
         (orbits (input-to-orbits lines)))
    (case part
      (1 (count-total-orbits orbits))
      (2 (find-number-of-orbital-transfers orbits 'you 'san))
      (otherwise (error "`part' must be either 1 or 2")))))
