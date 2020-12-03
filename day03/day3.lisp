(defpackage :aoc2019.day3
  (:use :cl
        :split-sequence))

(in-package :aoc2019.day3)

(defun lay-wire (grid wire-num curr-x curr-y axis arith-func count total-distance)
  (let ((changing-coord (if (eq axis :x) curr-x curr-y)))
    (loop for i = changing-coord then (funcall arith-func i 1)
          for segment-distance = 1 then (1+ segment-distance)
          until (= i (funcall arith-func changing-coord count))
          finally (return-from lay-wire (let ((x (if (eq axis :x) i curr-x))
                                              (y (if (eq axis :y) i curr-y)))
                                          (values grid x y (+ total-distance (1- segment-distance)))))
          do (let* ((coords (if (eq axis :x)
                                (cons (funcall arith-func i 1) curr-y)
                                (cons curr-x (funcall arith-func i 1))))
                    (wire-table (gethash coords grid)))
               (if wire-table
                   (unless (gethash wire-num wire-table)
                     (setf (gethash wire-num wire-table) (+ total-distance segment-distance)))
                   (let ((new-wire-table (make-hash-table)))
                     (setf (gethash wire-num new-wire-table) (+ total-distance segment-distance))
                     (setf (gethash coords grid) new-wire-table)))))))

(defun lay-wire-up (grid wire-num curr-x curr-y count total-distance)
  (lay-wire grid wire-num curr-x curr-y :y #'- count total-distance))

(defun lay-wire-down (grid wire-num curr-x curr-y count total-distance)
  (lay-wire grid wire-num curr-x curr-y :y #'+ count total-distance))

(defun lay-wire-left (grid wire-num curr-x curr-y count total-distance)
  (lay-wire grid wire-num curr-x curr-y :x #'- count total-distance))

(defun lay-wire-right (grid wire-num curr-x curr-y count total-distance)
  (lay-wire grid wire-num curr-x curr-y :x #'+ count total-distance))

(defun parse-input (input-lines)
  (mapcar (lambda (line)
            (let ((split-line (split-sequence #\, line)))
              (mapcar (lambda (code)
                        (cons (elt code 0) (parse-integer (subseq code 1))))
                      split-line)))
          input-lines))

(defun process-paths (grid paths)
  (loop for wire-num = 1 then (1+ wire-num)
        for path in paths
        do (process-path grid path wire-num))
  grid)

(defun process-path (grid path wire-num)
  (let ((curr-x 0)
        (curr-y 0)
        (total-distance 0))
    (loop for (dir . distance) in path do
      (let ((wire-func (case dir
                         (#\U #'lay-wire-up)
                         (#\R #'lay-wire-right)
                         (#\D #'lay-wire-down)
                         (#\L #'lay-wire-left))))
        (multiple-value-bind (grid new-x new-y new-total-distance)
            (funcall wire-func grid wire-num curr-x curr-y distance total-distance)
          (declare (ignore grid))
          (setf curr-x new-x
                curr-y new-y
                total-distance new-total-distance)))))
  grid)

(defun get-distance-to-closest-intersection (grid)
  (let ((shortest-distance nil))
    (loop for (x . y) being the hash-keys of grid using (hash-value v)
          when (> (hash-table-count v) 1)
            do (let ((distance (+ (abs x) (abs y))))
                 (when (or (not shortest-distance) (< distance shortest-distance))
                   (setf shortest-distance distance))))
    shortest-distance))

(defun get-lowest-number-of-steps (grid)
  (let ((lowest-steps nil))
    (loop for steps-table being the hash-values of grid
          when (> (hash-table-count steps-table) 1)
            do (let ((steps (reduce #'+ (alexandria:hash-table-values steps-table))))
                 (when (or (not lowest-steps) (< steps lowest-steps))
                   (setf lowest-steps steps))))
    lowest-steps))

(defun main (&key (part 2))
  (let* ((lines (with-open-file (input "input.txt")
                  (loop for line = (read-line input nil)
                        while line
                        collect line)))
         (paths (parse-input lines))
         (processed-paths (process-paths (make-hash-table :test 'equal) paths)))
    (cond ((= part 1) (get-distance-to-closest-intersection processed-paths))
          ((= part 2) (get-lowest-number-of-steps processed-paths))
          (t (error "`part' must be either 1 or 2")))))
