(defpackage :day13
  (:use #:cl #:alexandria #:intcode-interpreter))

(in-package :day13)

(defun display-game (game-state)
  (let* ((keys (hash-table-keys game-state))
         (xs (mapcar #'car keys))
         (ys (mapcar #'cdr keys))
         (max-x (apply #'max xs))
         (max-y (apply #'max ys)))
    (loop for y from 0 to max-y do
      (loop for x from 0 to max-x do
        (let ((tile-id (gethash (cons x y) game-state 0)))
          (format t "~a" (case tile-id
                           (0 " ")
                           (1 "|")
                           (2 "X")
                           (3 "_")
                           (4 "o")
                           (otherwise (error (format nil "Invalid tile id: ~a" tile-id)))))))
      (format t "~%"))))

(defun update-game (game-state input)
  (loop for (x y tile-id) on input by #'cdddr do
    (setf (gethash (cons x y) game-state) tile-id))
  game-state)

(defun count-blocks (game-state)
  (count-if (lambda (id) (= id 2)) (hash-table-values game-state)))

(defun game-complete-p (game-state)
  (= 0 (count-blocks game-state)))

(defun play-game (computer)
  (setf (access-address computer 0) 2)
  (loop with game-state = (update-game (make-hash-table :test 'equal)
                                       (run-computer computer))
        when (game-complete-p game-state)
          return (gethash (cons -1 0) game-state)
        do (let* ((ball-x (ball-x game-state))
                  (paddle-x (paddle-x game-state))
                  (new-game-state (update-game game-state
                                               (run-computer computer
                                                             (list (cond ((< ball-x paddle-x) -1)
                                                                         ((> ball-x paddle-x) 1)
                                                                         (t 0)))))))
             (setf game-state new-game-state))))

(defun tile-x (game-state tile-id)
  (loop for ((x . y) . id) in (hash-table-alist game-state)
        when (= tile-id id)
          return x))

(defun ball-x (game-state)
  (tile-x game-state 4))

(defun paddle-x (game-state)
  (tile-x game-state 3))

(defun main (&key (part 2))
  (let ((computer (make-computer (parse-input (read-file-into-string "input.txt")))))
    (case part
      (1 (let ((game-state (update-game (make-hash-table :test 'equal)
                                        (run-computer computer))))
           (count-blocks game-state)))
      (2 (play-game computer))
      (otherwise (error "`part' must be either 1 or 2")))))
