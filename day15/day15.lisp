(defpackage :day15
  (:use #:cl
        #:alexandria
        #:intcode-interpreter))

(in-package :day15)

(defstruct node
  path
  path-length
  total-length-estimate)

(defun find-shortest-path (start goal successors heuristic-dist)
  (do (head-node		; node at head of open list
       path-to-extend         ; path to state currently visited
       current-state		; state currently visited
       dist-so-far		; length of this path
       extended-paths         ; list of newly extended paths
       (open			; list of all candidate nodes
        (list (make-node :path (list start)
                         :path-length 0
                         :total-length-estimate
                         (funcall heuristic-dist start goal))))
       (state-distances (make-hash-table :test #'equalp)))
      ((null open) nil)         ; if open list is empty, search fails
    ;;      (format t                 ; lets us watch how algorithm works
    ;;		"~%Open: ~s~%" open)
    (setq head-node (pop open))       ; get node at head of open list
    (setq path-to-extend (node-path head-node)) ; get path itself
    (setq current-state (car path-to-extend)) ; get state this path ends at
    (when (equalp current-state goal)
      (return head-node))	; success: return path and length found
    (setq dist-so-far (node-path-length head-node))
    (when (less-than dist-so-far (gethash current-state state-distances))
      (setf (gethash current-state state-distances) dist-so-far)
      (let (next-state
            next-dist-so-far
            (next-nodes nil))
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

(defun move-droid (com map pos dir)
  (let ((next-pos (case dir
                    (:north (cons (car pos) (1- (cdr pos))))
                    (:south (cons (car pos) (1+ (cdr pos))))
                    (:west (cons (1- (car pos)) (cdr pos)))
                    (:east (cons (1+ (car pos)) (cdr pos))))))
    (destructuring-bind (result)
        (run-computer com (list (case dir
                                  (:north 1)
                                  (:south 2)
                                  (:west 3)
                                  (:east 4))))
      (case result
        (0
         (setf (gethash next-pos map) :wall)
         pos)
        (1
         (setf (gethash next-pos map) :clear)
         next-pos)
        (2
         (setf (gethash next-pos map) :oxygen)
         next-pos)))))

(defun print-map (map pos)
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
          (let ((cell (if (equal (cons x y) pos)
                          :droid
                          (gethash (cons x y) map))))
            (format t "~a" (case cell
                             (:wall "#")
                             (:clear ".")
                             (:oxygen "X")
                             (:droid "D")
                             (otherwise " ")))))
        (format t "~%")))))

(defun get-neighbors (map pos)
  (mapcar (lambda (c) (cons c 1))
          (remove-if (lambda (coords) (eql (gethash coords map) :wall))
                     (list
                      (cons (car pos) (1- (cdr pos)))
                      (cons (car pos) (1+ (cdr pos)))
                      (cons (1- (car pos)) (cdr pos))
                      (cons (1+ (car pos)) (cdr pos))))))

(defun get-heuristic (start goal)
  (+ (abs (- (car start) (car goal)))
     (abs (- (cdr start) (cdr goal)))))

(defun find-path (map pos goal-pos)
  (let ((node (find-shortest-path pos
                                  goal-pos
                                  (curry #'get-neighbors map)
                                  #'get-heuristic)))
    (when node (reverse (node-path node)))))

(defun find-next-move (map pos goal-pos)
  (let ((path (find-path map pos goal-pos)))
    (when path
      (let ((next-pos (second path)))
        (cond ((> (car next-pos) (car pos)) :east)
              ((< (car next-pos) (car pos)) :west)
              ((> (cdr next-pos) (cdr pos)) :south)
              ((< (cdr next-pos) (cdr pos)) :north)
              (t (error "wat do")))))))

(defun get-unchecked-coords (map)
  (let ((coords '()))
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
              (unless (gethash (cons x y) map)
                (push (cons x y) coords))))))
    coords))

(defun get-clear-coords (map)
  (let ((coords '()))
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
            (when (eql (gethash (cons x y) map) :clear)
              (push (cons x y) coords))))))
    coords))

(defun fill-in-the-blanks (com map pos)
  (loop for coords in (get-unchecked-coords map)
        with new-pos = pos do
    (loop with dir
          until (equal new-pos coords) do
            (setf dir (find-next-move map new-pos coords))
            (unless dir
              (return))
            (setf new-pos (move-droid com map new-pos dir)))))

(defun outline-map (com map minimum-map-width)
  (loop for pos = (cons 0 0)
          then (move-droid com map pos next-move)
        for next-move = (find-next-move map pos (cons (floor minimum-map-width 2) 0))
          then (find-next-move map pos (cons (floor minimum-map-width 2) 0))
        unless next-move
          return pos))

(defun main (&key (part 2))
  (let ((computer (make-computer (parse-input (read-file-into-string "input.txt"))))
        (map (make-hash-table :test 'equal)))
    (fill-in-the-blanks computer map (outline-map computer map 1000))
    (let ((oxygen-coords (loop for k being the hash-keys of map using (hash-value v)
                               when (eql v :oxygen)
                                 return k)))
      (case part
        (1 (length (cdr (find-path map '(0 . 0) oxygen-coords))))
        (2 (apply #'max
                  (mapcar (lambda (coords)
                            (length (cdr (find-path map oxygen-coords coords))))
                          (get-clear-coords map))))
        (otherwise (error "`part' must be either 1 or 2"))))))
