(defpackage #:day20
  (:use #:cl #:alexandria))

(in-package #:day20)

(defvar *r*)

(defun dijkstra-short-path (begin end neighbors)
  (let (*r*)
    (paths begin end 0 (list begin) neighbors)
    (caar (sort *r* #'< :key #'cadr))))

(defun paths (begin end cost result neighbors)
  (if (equal begin end)
      (push (list (reverse result) cost) *r*)
      (loop for linked in (funcall neighbors begin) do
        (unless (position linked result :test 'equal)
          (paths linked end (1+ cost) (cons linked result) neighbors)))))

(defun parse-input (input)
  (loop with map = (make-hash-table :test 'equal)
        for line in (str:lines input)
        for y = 0 then (1+ y) do
          (loop for c across line
                for x = 0 then (1+ x)
                do (setf (gethash (cons x y) map) c))
        finally (return map)))

(defun print-map (map &optional route)
  (when (> (hash-table-count map) 0)
    (let* ((keys (hash-table-keys map))
           (xs (mapcar #'car keys))
           (ys (mapcar #'cdr keys))
           (max-x (apply #'max xs))
           (max-y (apply #'max ys))
           (min-x (apply #'min xs))
           (min-y (apply #'min ys)))
      (loop for y from min-y to max-y do
        (loop for x from min-x to max-x do
          (format t "~a" (if (position (cons x y) route :test 'equal)
                             #\o
                             (gethash (cons x y) map))))
        (format t "~%")))))

(defun get-neighbors (map pos)
  (let* ((initial-neighbors (remove-if (lambda (coords) (or (char= (gethash coords map #\#) #\#)
                                                            (string= (get-portal-chars map coords)
                                                                     "AA")
                                                            (string= (get-portal-chars map coords)
                                                                     "ZZ")))
                                       (list
                                        (cons (car pos) (1- (cdr pos)))
                                        (cons (car pos) (1+ (cdr pos)))
                                        (cons (1- (car pos)) (cdr pos))
                                        (cons (1+ (car pos)) (cdr pos))))))
    (mapcar (lambda (coords)
              (if (char<= #\A (gethash coords map) #\Z)
                  (get-opposite-portal-coords map coords)
                  coords))
            initial-neighbors)))

(defun get-opposite-portal-coords (map pos)
  (let* ((portal-chars (get-portal-chars map pos))
         (all-other-portal-coords (let (other-portals)
                                    (maphash (lambda (k v)
                                               (when (char<= #\A v #\Z)
                                                 (push k other-portals)))
                                             map)
                                    other-portals)))
    (loop for coords in all-other-portal-coords
          when (and (not (equal pos coords))
                    (string= (get-portal-chars map coords) portal-chars))
            do
               (let ((opposite-coords (find-if (lambda (coords)
                                                 (char= (gethash coords map #\#) #\.))
                                               (list
                                                (cons (car coords) (1- (cdr coords)))
                                                (cons (car coords) (1+ (cdr coords)))
                                                (cons (1- (car coords)) (cdr coords))
                                                (cons (1+ (car coords)) (cdr coords))))))
                 (when opposite-coords
                   (return opposite-coords))))))

(defun get-portal-chars (map pos)
  (map 'string
       (lambda (coords) (gethash coords map))
       (remove-if-not (lambda (coords) (char<= #\A (gethash coords map #\.) #\Z))
                      (list
                       (cons (car pos) (1- (cdr pos)))
                       (cons (1- (car pos)) (cdr pos))
                       (cons (car pos) (cdr pos))
                       (cons (car pos) (1+ (cdr pos)))
                       (cons (1+ (car pos)) (cdr pos))))))

(defun get-heuristic (start goal)
  (if (= (abs (- (cdr start) (cdr goal))) 0)
      0
      (/ 1000
         (+ (abs (- (car start) (car goal)))
            (abs (- (cdr start) (cdr goal)))))))

(defun find-path (map pos goal-pos)
  (dijkstra-short-path pos goal-pos (curry #'get-neighbors map)))

(defun find-by-chars (map chars)
  (loop for coords being the hash-keys of map
        when (string= (get-portal-chars map coords) chars) do
          (let ((walkable-coords (find-if (lambda (coords)
                                            (char= (gethash coords map #\#) #\.))
                                          (list
                                           (cons (car coords) (1- (cdr coords)))
                                           (cons (car coords) (1+ (cdr coords)))
                                           (cons (1- (car coords)) (cdr coords))
                                           (cons (1+ (car coords)) (cdr coords))))))
            (when walkable-coords
              (return walkable-coords)))))

(defun find-start (map)
  (find-by-chars map "AA"))

(defun find-end (map)
  (find-by-chars map "ZZ"))

(defun main (&key (part 1))
  (let ((map (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (length (cdr (find-path map (find-start map) (find-end map)))))
      (2 (error "unimplemented"))
      (otherwise (error "`part' must be either 1 or 2")))))
