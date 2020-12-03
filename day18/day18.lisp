(defpackage #:aoc2019.day18
  (:use #:cl #:alexandria))

(in-package #:aoc2019.day18)

(defun parse-input (input)
  (loop with map = (make-hash-table :test 'equal)
        for line in (str:lines input)
        for y = 0 then (1+ y) do
          (loop for c across line
                for x = 0 then (1+ x)
                do (setf (gethash (cons x y) map) c))
        finally (return map)))

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
       (state-distances (make-hash-table :test #'equal)))
      ((null open) nil)         ; if open list is empty, search fails
    (setq head-node (pop open))       ; get node at head of open list
    (setq path-to-extend (node-path head-node)) ; get path itself
    (setq current-state (car path-to-extend)) ; get state this path ends at
    (when (equal current-state goal)
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
  (declare (fixnum x)
           ((or fixnum null) y))
  (or (null y) (< x y)))

(defun get-neighbors (map pos)
  (let ((neighbors (remove-if (lambda (coords)
                                (let ((tile (gethash coords map #\#)))
                                  (char= tile #\#)))
                              (list
                               (cons (car pos) (1- (cdr pos)))
                               (cons (car pos) (1+ (cdr pos)))
                               (cons (1- (car pos)) (cdr pos))
                               (cons (1+ (car pos)) (cdr pos))))))
    (mapcar (rcurry #'cons 1) neighbors)))

(defun get-distance (start end)
  (+ (abs (- (car start) (car end)))
     (abs (- (cdr start) (cdr end)))))

(defun find-pos (map char)
  (loop for coords being the hash-keys of map using (hash-value tile)
        when (char= tile char)
          return coords))

(defun find-all-pos (map char)
  (loop for coords being the hash-keys of map using (hash-value tile)
        when (char= tile char)
          collect coords))

(defun get-key-positions (map)
  (loop for tile being the hash-values of map using (hash-key pos)
        when (lower-case-p tile)
          collect pos))

(defun get-keys (map)
  (loop for tile being the hash-values of map
        when (lower-case-p tile)
          collect tile))

(defun get-key-routes (map)
  (let* ((keys (coerce (append (find-all-pos map #\@) (get-key-positions map)) 'vector))
         (key-count (length keys)))
    (loop for i below key-count
          append (loop for j from i below key-count
                       collecting (list (aref keys i) (aref keys j))))))

(defstruct (path :conc-name) path-end path-length key doors)

(defun doors-along (map path)
  (loop for coords in path
        for tile = (gethash coords map)
        when (upper-case-p tile)
          collect tile))

(defun get-paths-between-keys (map)
  (let ((paths (make-hash-table :test 'equal)))
    (loop for (start end) in (get-key-routes map) do
      (when-let* ((node (find-shortest-path start
                                            end
                                            (curry #'get-neighbors map)
                                            #'get-distance)))
        (unless (equal start end)
          (let ((path (make-path :path-end end
                                 :path-length (node-path-length node)
                                 :key (gethash end map)
                                 :doors (doors-along map (node-path node))))
                (start-char (gethash start map)))
            (push path (gethash start paths))
            (unless (char= start-char #\@)
              (push (make-path :path-end start
                               :path-length (path-length path)
                               :key (gethash start map)
                               :doors (doors path))
                    (gethash end paths)))))))
    paths))

(defun tuple-compare (comparison-functions)
  (lambda (left right)
    (loop for fn in comparison-functions
          for x in left
          for y in right
            thereis (funcall fn x y)
          until (funcall fn y x))))

(defun distance-to-collect-keys (map starts paths &optional
                                                    (remaining-keys (get-keys map))
                                                    (cache (make-hash-table :test 'equal)))
  (let ((starts (sort starts
                      (tuple-compare (list #'< #'<))
                      :key (lambda (c)
                             (destructuring-bind (x . y) c
                               (list x y))))))
    (loop for start in starts
          minimizing
          (cond ((null remaining-keys) 0)
                (t (or (gethash (cons starts remaining-keys) cache)
                       (let ((reachable-keys (reachable-keys (gethash start paths) remaining-keys)))
                         (if reachable-keys
                             (setf (gethash (cons starts remaining-keys) cache)
                                   (loop for path in reachable-keys
                                         minimizing (+ (path-length path)
                                                       (distance-to-collect-keys
                                                        map
                                                        (cons (path-end path)
                                                              (remove start starts :test 'equal))
                                                        paths
                                                        (remove (key path) remaining-keys)
                                                        cache))))
                             most-positive-fixnum))))))))

(defun reachable-keys (paths remaining-keys)
  (remove-if-not (lambda (path)
                   (position (key path) remaining-keys))
                 (remove-if (lambda (path)
                              (some #'(lambda (door)
                                        (position (char-downcase door) remaining-keys))
                                    (doors path)))
                            paths)))

(defun main (&key (part 2))
  (let ((map (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (distance-to-collect-keys map (find-all-pos map #\@) (get-paths-between-keys map)))
      (2
       (let ((center-pos (find-pos map #\@)))
         (setf (gethash center-pos map) #\#
               (gethash (cons (1+ (car center-pos)) (cdr center-pos)) map) #\#
               (gethash (cons (1- (car center-pos)) (cdr center-pos)) map) #\#
               (gethash (cons (car center-pos) (1+ (cdr center-pos))) map) #\#
               (gethash (cons (car center-pos) (1- (cdr center-pos))) map) #\#
               (gethash (cons (1- (car center-pos)) (1- (cdr center-pos))) map) #\@
               (gethash (cons (1+ (car center-pos)) (1- (cdr center-pos))) map) #\@
               (gethash (cons (1+ (car center-pos)) (1+ (cdr center-pos))) map) #\@
               (gethash (cons (1- (car center-pos)) (1+ (cdr center-pos))) map) #\@)
         (distance-to-collect-keys map (find-all-pos map #\@) (get-paths-between-keys map))))
      (otherwise (error "`part' must be either 1 or 2")))))
