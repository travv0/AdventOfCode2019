(defpackage #:day18
  (:use #:cl #:alexandria))

(in-package #:day18)

(declaim (optimize speed (debug 1) safety))

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

(defun find-shortest-path (start goal-func successors heuristic-dist)
  (do (head-node		; node at head of open list
       path-to-extend         ; path to state currently visited
       current-state		; state currently visited
       dist-so-far		; length of this path
       (open			; list of all candidate nodes
        (list (make-node :path (list start)
                         :path-length 0
                         :total-length-estimate
                         (funcall heuristic-dist start))))
       (state-distances (make-hash-table :test #'equalp)))
      ((null open) nil)         ; if open list is empty, search fails
    (setq head-node (pop open))       ; get node at head of open list
    (setq path-to-extend (node-path head-node)) ; get path itself
    (setq current-state (car path-to-extend)) ; get state this path ends at
    (when (funcall goal-func current-state)
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
                        (funcall heuristic-dist next-state))))
                   open
                   #'<
                   :key #'node-total-length-estimate))))))))

;;; Here the y argument may be nil, which is treated like infinity.

(defun less-than (x y)
  (declare (fixnum x)
           ((or fixnum null) y))
  (or (null y) (< x y)))

(defun get-neighbors (map pos)
  (destructuring-bind (pos . keys) pos
    (declare ((or cons null) keys))
    (let ((neighbors (remove-if (lambda (coords)
                                  (let ((tile (gethash coords map #\#)))
                                    (or (char= tile #\#)
                                        (and (char<= #\A tile #\Z)
                                             (not (position tile keys :test 'equalp))))))
                                (list
                                 (cons (car pos) (1- (cdr pos)))
                                 (cons (car pos) (1+ (cdr pos)))
                                 (cons (1- (car pos)) (cdr pos))
                                 (cons (1+ (car pos)) (cdr pos))))))
      (mapcar (lambda (coords)
                (let ((tile (gethash coords map #\.)))
                  (cons (cons coords (if (and (char<= #\a tile #\z)
                                              (not (position tile keys)))
                                         (cons tile keys)
                                         keys))
                        1)))
              neighbors))))

(defun get-heuristic (key-coords number-of-keys pos)
  (+ (- number-of-keys (length (cdr pos)))
     (loop for coords in key-coords
           minimizing (get-distance (car pos) coords))))

(defun get-distance (start end)
  (+ (abs (- (car start) (car end)))
     (abs (- (cdr start) (cdr end)))))

(defun find-path (map pos)
  (let* ((key-coords (loop for coords being the hash-keys of map
                             using (hash-value tile)
                           when (lower-case-p tile)
                             collecting coords))
         (total-keys (length key-coords))
         (node (find-shortest-path (cons pos '())
                                   (compose (curry #'= total-keys) #'length #'cdr)
                                   (curry #'get-neighbors map)
                                   (curry #'get-heuristic key-coords total-keys))))
    (when node (reverse (node-path node)))))

(defun find-start (map)
  (loop for coords being the hash-keys of map using (hash-value tile)
        when (char= tile #\@)
          return coords))

(defun main (&key (part 1))
  (let ((map (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (length (cdr (find-path map (find-start map)))))
      (2 (error "unimplemented"))
      (otherwise (error "`part' must be either 1 or 2")))))
