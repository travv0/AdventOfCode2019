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

(defun get-route-segments (route &optional previous-segments)
  (when (> (length route) 2)
    (loop with route = route
          for i = 2 then (+ i 2)
          when (> i 10)
            return '()
          when (> (+ i 3) (length route))
            return (list route)
          do
             (let* ((tail-segments (get-route-segments (subseq route (+ i 2))
                                                       (cons (subseq route 0 (+ i 2)) previous-segments)))
                    (segments (cons (subseq route 0 (+ i 2)) tail-segments))
                    (unduped-segments (undup-segments (append segments previous-segments))))
               (unless (or (> (length unduped-segments) 3)
                           (some (lambda (segment) (> (* 2 (length (mapcar #'write-to-string segment)))
                                                      20))
                                 unduped-segments))
                 (when (and (= 3 (length unduped-segments))
                            (equal route (flatten segments))
                            (every (lambda (segment) (<= (* 2 (length (mapcar #'write-to-string segment)))
                                                         20))
                                   unduped-segments))
                   (return segments)))))))

(defun get-main-routine (segments)
  (let ((unduped-segments (undup-segments segments)))
    (mapcar (lambda (s) (code-char (+ (position s unduped-segments :test 'equal)
                                      (char-code #\A))))
            segments)))

(defun undup-segments (segments)
  (remove-duplicates segments :test 'equal :from-end t))

(defun get-computer-inputs (scaffolding &key continuous-video)
  (let* ((segments (get-route-segments (get-route scaffolding)))
         (unduped-segments (undup-segments segments))
         (routine (get-main-routine segments))
         (char-segments (mapcar (lambda (segment)
                                  (flatten (mapcar (lambda (x)
                                                     (if (numberp x)
                                                         (coerce (write-to-string x) 'list)
                                                         x))
                                                   (intersperse #\, segment))))
                                unduped-segments))
         (routine-input (mapcar #'char-code
                                (append (intersperse #\, routine) (list #\Newline))))
         (segment-inputs (mapcar (lambda (s)
                                   (mapcar #'char-code
                                           (append s (list #\Newline))))
                                 char-segments)))
    (append (flatten (cons routine-input segment-inputs))
            (mapcar #'char-code (list (if continuous-video #\y #\n) #\Newline)))))

(defun intersperse (elem list)
  (if (<= (length list) 1)
      list
      (append
       (list (car list) elem)
       (intersperse elem (cdr list)))))

(defun main (&key (part 2))
  (let* ((computer (make-computer
                    (parse-input (read-file-into-string "input.txt"))))
         (scaffolding (convert-computer-output (run-computer computer))))
    (case part
      (1 (print (calculate-alignment-parameters scaffolding)))
      (2
       (reset-computer computer)
       (setf (access-address computer 0) 2)
       (let* ((inputs (get-computer-inputs scaffolding))
              (output (run-computer computer inputs))
              (last-number (car (last output))))
         (if (> last-number 255)
             (print last-number)
             (format t "~a~%" (map 'string #'code-char output)))))
      (otherwise (error "`part' must be either 1 or 2")))))
