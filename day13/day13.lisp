(defpackage :day13
  (:use #:cl #:alexandria #:intcode-interpreter))

(in-package :day13)

(defun display-game (tiles)
  (let* ((keys (hash-table-keys tiles))
         (xs (mapcar #'car keys))
         (ys (mapcar #'cdr keys))
         (min-x (apply #'min xs))
         (max-x (apply #'max xs))
         (min-y (apply #'min ys))
         (max-y (apply #'max ys)))
    (loop for y from min-y to max-y do
      (loop for x from min-x to max-x do
        (let ((tile-id (gethash (cons x y) tiles 0)))
          (format t "~a" (case tile-id
                           (0 " ")
                           (1 "|")
                           (2 "X")
                           (3 "_")
                           (4 "o")
                           (otherwise (error (format nil "Invalid tile id: ~a" tile-id)))))))
      (format t "~%"))))

(defun update-game (input)
  (let ((tiles (make-hash-table :test 'equal)))
    (loop for (x y tile-id) on input by #'cdddr do
      (setf (gethash (cons x y) tiles) tile-id))
    tiles))

(defun main ()
  (let* ((computer (make-computer (parse-input (read-file-into-string "input.txt"))))
         (game-state (update-game (run-computer computer))))
    (count-if (lambda (id) (= id 2)) (hash-table-values game-state))))
