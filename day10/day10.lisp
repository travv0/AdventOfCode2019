(defpackage :aoc2019.day10
  (:use :cl :split-sequence :alexandria))

(in-package :aoc2019.day10)

(defvar *asteroid-map*)

(defun parse-input (input)
  (let ((lines (str:lines input)))
    (make-array (list (length lines) (length (nth 0 lines)))
                :initial-contents lines)))

(defun get-coord (x y)
  (aref *asteroid-map* y x))

(defun asteroid-p (x y)
  (char= (get-coord x y) #\#))

(defun get-number-of-visible-asteroids-from-point (origin-x origin-y)
  (let ((visible-asteroid-coords '()))
    (loop for y from 0 below (array-dimension *asteroid-map* 0) do
      (loop for x from 0 below (array-dimension *asteroid-map* 1) do
        (when (and (asteroid-p x y)
                   (not (and (= origin-x x) (= origin-y y)))
                   (notany (lambda (a)
                             (asteroid-blocked-or-blocking-p (list origin-x origin-y)
                                                             (list x y)
                                                             a))
                           visible-asteroid-coords))
          (setf visible-asteroid-coords (cons (list x y) visible-asteroid-coords)))))
    (length visible-asteroid-coords)))

(defun asteroid-blocked-or-blocking-p (origin-asteroid asteroid-1 asteroid-2)
  (let* ((origin-x (first origin-asteroid))
         (origin-y (second origin-asteroid))
         (x1 (first asteroid-1))
         (y1 (second asteroid-1))
         (x2 (first asteroid-2))
         (y2 (second asteroid-2))
         (dx1 (- origin-x x1))
         (dy1 (- origin-y y1))
         (dx2 (- origin-x x2))
         (dy2 (- origin-y y2)))
    (cond ((or (and (= dx1 0) (= dx2 0) (eql (< dy1 0) (< dy2 0)))
               (and (= dy1 0) (= dy2 0) (eql (< dx1 0) (< dx2 0))))
           t)
          ((and (/= dx2 0) (/= dy2 0)) (and (eql (< dx1 0) (< dx2 0))
                                            (eql (< dy1 0) (< dy2 0))
                                            (= (/ dx1 dx2) (/ dy1 dy2)))))))

(defun get-optimal-asteroid ()
  (let ((optimal-asteroid)
        (max-visible-asteroids 0))
    (loop for y from 0 below (array-dimension *asteroid-map* 0) do
      (loop for x from 0 below (array-dimension *asteroid-map* 1) do
        (when (asteroid-p x y)
          (let ((visible-asteroids (get-number-of-visible-asteroids-from-point x y)))
            (when (> visible-asteroids max-visible-asteroids)
              (setf optimal-asteroid (list x y)
                    max-visible-asteroids visible-asteroids))))))
    (values optimal-asteroid max-visible-asteroids)))

(defun calculate-angle (origin-asteroid dest-asteroid)
  (let* ((origin-x (first origin-asteroid))
         (origin-y (second origin-asteroid))
         (dest-x (first dest-asteroid))
         (dest-y (second dest-asteroid))
         (dx (- origin-x dest-x))
         (dy (- origin-y dest-y)))
    (loop for angle = (- (* (atan dy dx)
                            (/ 180 pi))
                         90)
            then (+ angle 360)
          when (> angle 0)
            return angle)))

(defun calculate-distance (origin-asteroid dest-asteroid)
  (let* ((origin-x (first origin-asteroid))
         (origin-y (second origin-asteroid))
         (dest-x (first dest-asteroid))
         (dest-y (second dest-asteroid))
         (dx (- origin-x dest-x))
         (dy (- origin-y dest-y)))
    (sqrt (+ (expt dx 2) (expt dy 2)))))

(defun build-asteroid-coord-list (exclude-x exclude-y)
  (let ((asteroid-coord-list '()))
    (loop for y from 0 below (array-dimension *asteroid-map* 0) do
      (loop for x from 0 below (array-dimension *asteroid-map* 1) do
        (when (and (not (and (= exclude-x x) (= exclude-y y)))
                   (asteroid-p x y))
          (setf asteroid-coord-list (cons (list x y) asteroid-coord-list)))))
    asteroid-coord-list))

(defun build-angles-and-distances-list (origin-coords coords-list)
  (let ((coords-hash (make-hash-table)))
    (mapc (lambda (coords)
            (let ((angle (calculate-angle origin-coords coords)))
              (setf (gethash angle coords-hash)
                    (cons (list :coords coords
                                :angle (calculate-angle origin-coords coords)
                                :distance (calculate-distance origin-coords coords))
                          (gethash angle coords-hash '())))))
          coords-list)
    (sort (mapcar (lambda (l)
                    (sort l (lambda (e1 e2) (< (getf e1 :distance)
                                               (getf e2 :distance)))))
                  (hash-table-values coords-hash))
          (lambda (l1 l2) (< (getf (car l1) :angle)
                             (getf (car l2) :angle))))))

(defun flatten-ordered-destruction-list (angles-and-distances-list)
  (let ((flattened-list '()))
    (loop for i = 0 then (mod (1+ i) (length angles-and-distances-list))
          for e = (nth i angles-and-distances-list)
          until (every #'not angles-and-distances-list)
          when e
            do (setf flattened-list (cons (car e) flattened-list)
                     (nth i angles-and-distances-list) (cdr e))
          finally (return (reverse flattened-list)))))

(defun get-200th-asteroid-to-be-destroyed (origin-coords)
  (let ((x (first origin-coords))
        (y (second origin-coords)))
    (destructuring-bind (result-x result-y)
        (getf
         (nth 199
              (flatten-ordered-destruction-list
               (build-angles-and-distances-list origin-coords
                                                (build-asteroid-coord-list x y))))
         :coords)
      (+ (* result-x 100) result-y))))

(defun main (&key (part 2))
  (let ((*asteroid-map* (parse-input (read-file-into-string "input.txt"))))
    (multiple-value-bind (optimal-asteroid visible-asteroids) (get-optimal-asteroid)
      (case part
        (1 visible-asteroids)
        (2 (get-200th-asteroid-to-be-destroyed optimal-asteroid))
        (otherwise (error "`part' must be either 1 or 2"))))))

(defun tests ()
  (let ((*asteroid-map* (parse-input ".#..#
.....
#####
....#
...##")))
    (test '(3 4) 8))
  (let ((*asteroid-map* (parse-input "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")))
    (test '(5 8) 33))
  (let ((*asteroid-map* (parse-input "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")))
    (test '(1 2) 35))
  (let ((*asteroid-map* (parse-input ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")))
    (test '(6 3) 41))
  (let ((*asteroid-map* (parse-input ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")))
    (test '(11 13) 210)))

(defun test (expected-coords expected-count)
  (format t "Best is ~a with ~a other asteroids detected." expected-coords expected-count)
  (multiple-value-bind (coords count) (get-optimal-asteroid)
    (if (and (equal coords expected-coords) (= count expected-count))
        (format t "  Passed.~%")
        (format t "  FAILED!~%Got ~a with ~a other asteroids detected.~%~%" coords count))))
