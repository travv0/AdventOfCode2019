(defpackage #:day20
  (:use #:cl #:alexandria))

(in-package #:day20)

(defstruct node
  path
  path-length
  total-length-estimate)

(defun find-shortest-path (start goal successors heuristic-dist)
  (do (head-node		; node at head of open list
       path-to-extend         ; path to state currently visited
       current-state		; state currently visited
       dist-so-far		; length of this path
       (open			; list of all candidate nodes
        (list (make-node :path (list start)
                         :path-length 0
                         :total-length-estimate
                         (funcall heuristic-dist start goal))))
       (state-distances (make-hash-table :test #'equalp)))
      ((null open) nil)         ; if open list is empty, search fails
    (setq head-node (pop open))       ; get node at head of open list
    (setq path-to-extend (node-path head-node)) ; get path itself
    (setq current-state (car path-to-extend)) ; get state this path ends at
    (when (equalp current-state goal)
      (return head-node))	; success: return path and length found
    (setq dist-so-far (node-path-length head-node))
    (when (less-than dist-so-far (gethash current-state state-distances))
      (setf (gethash current-state state-distances) dist-so-far)
      (let (next-state
            next-dist-so-far)
        (dolist (pair (funcall successors current-state))
          (setq next-state (car pair))
          (setq next-dist-so-far (+ (cdr pair) dist-so-far))
          (when (less-than next-dist-so-far
                           (gethash next-state state-distances))
            (setf open
                  (merge
                   'list
                   (list
                    (make-node
                     :path (cons next-state path-to-extend)
                     :path-length next-dist-so-far
                     :total-length-estimate
                     (+ next-dist-so-far
                        (funcall heuristic-dist next-state goal))))
                   open
                   #'<
                   :key #'node-total-length-estimate))))))))

;;; Here the y argument may be nil, which is treated like infinity.

(defun less-than (x y)
  (or (null y) (< x y)))

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
           (max-y (apply #'max ys)))
      (loop for y from 0 to max-y do
        (loop for x from 0 to max-x do
          (format t "~a" (let ((r (find-if (lambda (c) (equal (car c) (cons x y))) route)))
                           (if r
                               (mod (cdr r) 10)
                               (gethash (cons x y) map)))))
        (format t "~%")))))

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
  (get-distance start goal))

(defun get-distance (start end)
  (+ (abs (- (car start) (car end)))
     (abs (- (cdr start) (cdr end)))))

(defun find-path (map pos goal-pos)
  (let ((node (find-shortest-path pos
                                  goal-pos
                                  (curry #'get-neighbors map)
                                  #'get-heuristic)))
    (when node (reverse (node-path node)))))

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
              (cons (if (char<= #\A (gethash coords map) #\Z)
                        (get-opposite-portal-coords map coords)
                        coords)
                    1))
            initial-neighbors)))

(defun find-recursive-path (map pos goal-pos)
  (let ((node (find-shortest-path (cons pos 1)
                                  (cons goal-pos 1)
                                  (curry #'get-recursive-neighbors map)
                                  #'get-recursive-heuristic)))
    (when node (reverse (node-path node)))))

(defun get-recursive-neighbors (map pos)
  (let* ((initial-neighbors (remove-if (lambda (coords)
                                         (or (char= (gethash coords map #\#) #\#)
                                             (string= (get-portal-chars map coords)
                                                      "AA")
                                             (string= (get-portal-chars map coords)
                                                      "ZZ")))
                                       (let ((pos (car pos)))
                                         (list
                                          (cons (car pos) (1- (cdr pos)))
                                          (cons (car pos) (1+ (cdr pos)))
                                          (cons (1- (car pos)) (cdr pos))
                                          (cons (1+ (car pos)) (cdr pos)))))))
    (remove-if (compose (curry #'= 0) #'cdar)
               (mapcar (lambda (coords)
                         (cons (if (char<= #\A (gethash coords map) #\Z)
                                   (let ((opposite-portal (get-opposite-portal-coords map coords)))
                                     (cons opposite-portal (if (center-portal-p map (car pos) opposite-portal)
                                                               (1+ (cdr pos))
                                                               (1- (cdr pos)))))
                                   (cons coords (cdr pos)))
                               1))
                       initial-neighbors))))

(defun get-recursive-heuristic (start goal)
  (get-distance (car start) (car goal)))

(defun center-portal-p (map pos compare-to-pos)
  (when (> (hash-table-count map) 0)
    (let* ((keys (hash-table-keys map))
           (xs (mapcar #'car keys))
           (ys (mapcar #'cdr keys))
           (max-x (apply #'max xs))
           (max-y (apply #'max ys))
           (x (car pos))
           (y (cdr pos))
           (other-x (car compare-to-pos))
           (other-y (cdr compare-to-pos)))
      (> (apply #'min (list x y (- max-x x) (- max-y y)))
         (apply #'min (list other-x other-y (- max-x other-x) (- max-y other-y)))))))

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

(defun main (&key (part 2))
  (let ((map (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (length (cdr (find-path map (find-start map) (find-end map)))))
      (2 (length (cdr (find-recursive-path map (find-start map) (find-end map)))))
      (otherwise (error "`part' must be either 1 or 2")))))
