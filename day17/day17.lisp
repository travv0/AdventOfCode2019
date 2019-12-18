(defpackage :day17
  (:use #:cl
        #:alexandria
        #:intcode-interpreter))

(in-package :day17)

(defun convert-computer-output (output)
  (let ((lines (str:lines (map 'string #'code-char output) :omit-nulls t)))
    (make-array (list (length lines) (length (car lines)))
                :initial-contents lines)))

(defun calculate-alignment-parameters (scaffolding)
  (loop for y from 0 below (array-dimension scaffolding 0)
        sum (loop for x from 0 below (second (array-dimensions scaffolding))
                  when (is-intersection scaffolding x y)
                    sum (* x y))))

(defun is-intersection (scaffolding x y)
  (when (away-from-edge scaffolding x y)
    (every (lambda (c) (char= c #\#))
           (list (aref scaffolding y x)
                 (aref scaffolding (1- y) x)
                 (aref scaffolding (1+ y) x)
                 (aref scaffolding y (1- x))
                 (aref scaffolding y (1+ x))))))

(defun get-route (scaffolding)
  (let ((robot-pos
          (block outer
            (loop for y from 0 below (array-dimension scaffolding 0) do
              (loop for x from 0 below (second (array-dimensions scaffolding))
                    when (char= (aref scaffolding y x) #\^)
                      do (return-from outer (cons x y))))))
        (robot-dir :up)
        (route '()))
    (loop do
      (when (next-spot-is-death scaffolding robot-pos robot-dir)
        (let ((new-direction (get-new-direction scaffolding robot-pos robot-dir)))
          (if new-direction
              (destructuring-bind (new-dir . turn) new-direction
                (setf robot-dir new-dir)
                (push turn route))
              (return (reverse route)))))
      (destructuring-bind  (new-pos . distance)
          (get-end-of-straightaway scaffolding robot-pos robot-dir)
        (setf robot-pos new-pos)
        (push distance route)))))

(defun next-spot-is-death (scaffolding robot-pos robot-dir)
  (destructuring-bind (x . y) robot-pos
    (case robot-dir
      (:up (or (= y 0) (char= (aref scaffolding (1- y) x) #\.)))
      (:left (or (= x 0) (char= (aref scaffolding y (1- x)) #\.)))
      (:down (or (= y (1- (array-dimension scaffolding 0)))
                 (char= (aref scaffolding (1+ y) x) #\.)))
      (:right (or (= x (1- (second (array-dimensions scaffolding))))
                  (char= (aref scaffolding y (1+ x)) #\.))))))

(defun get-end-of-straightaway (scaffolding robot-pos robot-dir)
  (destructuring-bind (x . y) robot-pos
    (case robot-dir
      (:up (loop for new-y = (1- y) then (1- new-y)
                 when (next-spot-is-death scaffolding (cons x new-y) robot-dir)
                   return (cons (cons x new-y) (- y new-y))))
      (:left (loop for new-x = (1- x) then (1- new-x)
                   when (next-spot-is-death scaffolding (cons new-x y) robot-dir)
                     return (cons (cons new-x y) (- x new-x))))
      (:down (loop for new-y = (1+ y) then (1+ new-y)
                   when (next-spot-is-death scaffolding (cons x new-y) robot-dir)
                     return (cons (cons x new-y) (- new-y y))))
      (:right (loop for new-x = (1+ x) then (1+ new-x)
                    when (next-spot-is-death scaffolding (cons new-x y) robot-dir)
                      return (cons (cons new-x y) (- new-x x)))))))

(defun get-new-direction (scaffolding robot-pos robot-dir)
  (destructuring-bind (x . y) robot-pos
    (case robot-dir
      (:up (cond ((and (> x 0) (char= (aref scaffolding y (1- x)) #\#))
                  (cons :left #\L))
                 ((and (< x (1- (second (array-dimensions scaffolding))))
                       (char= (aref scaffolding y (1+ x)) #\#))
                  (cons :right #\R))))
      (:left (cond ((and (> y 0) (char= (aref scaffolding (1- y) x) #\#))
                    (cons :up #\R))
                   ((and (< y (1- (array-dimension scaffolding 0)))
                         (char= (aref scaffolding (1+ y) x) #\#))
                    (cons :down #\L))))
      (:down (cond ((and (> x 0) (char= (aref scaffolding y (1- x)) #\#))
                    (cons :left #\R))
                   ((and (< x (1- (second (array-dimensions scaffolding))))
                         (char= (aref scaffolding y (1+ x)) #\#))
                    (cons :right #\L))))
      (:right (cond ((and (> y 0) (char= (aref scaffolding (1- y) x) #\#))
                     (cons :up #\L))
                    ((and (< y (1- (array-dimension scaffolding 0)))
                          (char= (aref scaffolding (1+ y) x) #\#))
                     (cons :down #\R)))))))

(defun away-from-edge (scaffolding x y)
  (and (> x 0)
       (> y 0)
       (< x (1- (second (array-dimensions scaffolding))))
       (< y (1- (array-dimension scaffolding 0)))))

(defun get-route-segments (route)
  (loop with route = route
        with segments = '()
        with longest-segment = (list (car route))
        for i = 0 then (+ i 2)
        when (> i (length route)) do
          (return (reverse segments))
        if (search (subseq route 0 i) (cdr route)) do
          (setf longest-segment (subseq route 0 i))
        else do
          (when (> (length longest-segment) 1)
            (pushnew longest-segment segments :test 'equal))
          (setf route (subseq route (- i 2))
                longest-segment (list (car route))
                i 2)))

(defun main (&key (part 1))
  (let* ((computer (make-computer
                    (parse-input (read-file-into-string "input.txt"))))
         (scaffolding (convert-computer-output (run-computer computer))))
    (case part
      (1 (calculate-alignment-parameters scaffolding))
      (2 (error "unimplemented"))
      (otherwise (error "`part' must be either 1 or 2")))))
