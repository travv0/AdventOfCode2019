(defpackage #:day12
  (:use #:travv0.prelude
        #:lparallel))

(in-package #:day12)

(declaim (optimize speed))

(unless *kernel*
  (setf *kernel* (lparallel:make-kernel 4)))

(defun make-moon (x y z)
  (list :x (list :pos x :vel 0)
        :y (list :pos y :vel 0)
        :z (list :pos z :vel 0)))

(defun line-to-moon (line)
  (cl-ppcre:register-groups-bind (x y z)
      ("<x=([-\\d]+), y=([-\\d]+), z=([-\\d]+)>" line)
    (when (and x y z)
      (make-moon (parse-integer x)
                 (parse-integer y)
                 (parse-integer z)))))

(defun parse-input (input)
  (->> input
       str:lines
       (mapcar #'line-to-moon)))

(defun update (plist key fn)
  (let ((val (getf plist key))
        (new-list (copy-list plist)))
    (setf (getf new-list key)
          (funcall fn val))
    new-list))

(defun apply-gravity ((&whole moon-axis &key pos) (&key ((:pos other-pos))))
  (update moon-axis :vel
          (cond ((> pos other-pos) #'1-)
                ((< pos other-pos) #'1+)
                (t #'identity))))

(defun apply-gravities (moon-axis other-moon-axes)
  (reduce #'apply-gravity other-moon-axes :initial-value moon-axis))

(defun simulate-gravity (moon-axes)
  (labels ((f (moon-axes n)
             (if (> n 0)
                 (let ((rest-moon-axes (rest moon-axes)))
                   (f (append rest-moon-axes
                              (list (apply-gravities (first moon-axes) rest-moon-axes)))
                      (1- n)))
                 moon-axes)))
    (f moon-axes (length moon-axes))))

(defun apply-velocity (moon-axis)
  (update moon-axis :pos (lambda (pos) (+ pos (getf moon-axis :vel)))))

(defun step-once (moon-axes)
  (->> moon-axes
       simulate-gravity
       (mapcar #'apply-velocity)))

(defun run-simulation (moons num-of-steps)
  (labels ((f (xs ys zs n)
             (if (> n 0)
                 (f (step-once xs) (step-once ys) (step-once zs) (1- n))
                 (mapcar (lambda (x y z)
                           (list :x x :y y :z z))
                         xs ys zs))))
    (f (mapcar (lambda (moon) (getf moon :x)) moons)
       (mapcar (lambda (moon) (getf moon :y)) moons)
       (mapcar (lambda (moon) (getf moon :z)) moons)
       num-of-steps)))

(defun get-potential-energy ((&key ((:x (&key ((:vel x) 0))))
                                ((:y (&key ((:vel y) 0))))
                                ((:z (&key ((:vel z) 0))))))
  (+ (abs x) (abs y) (abs z)))

(defun get-kinetic-energy ((&key ((:x (&key ((:pos x) 0))))
                              ((:y (&key ((:pos y) 0))))
                              ((:z (&key ((:pos z) 0))))))
  (+ (abs x) (abs y) (abs z)))

(defun find-steps-until-repeated-state (moons)
  (bind ((xs (mapcar (lambda (moon) (getf moon :x)) moons))
         (ys (mapcar (lambda (moon) (getf moon :y)) moons))
         (zs (mapcar (lambda (moon) (getf moon :z)) moons))
         ((:labels f (moon-axes &optional initial-state (num-of-steps 0)))
          (if (equal moon-axes initial-state)
              num-of-steps
              (f (step-once moon-axes)
                 (or initial-state moon-axes)
                 (1+ num-of-steps)))))
    (reduce #'lcm (pmapcar #'f (list xs ys zs)))))

(defun main (&key (part 2))
  (let ((moons (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (-<>> moons
               (run-simulation <> 1000)
               (mapcar (lambda (moon) (* (get-potential-energy moon)
                                         (get-kinetic-energy moon))))
               (reduce #'+)))
      (2 (find-steps-until-repeated-state moons))
      (otherwise (error "`part' must be either 1 or 2")))))
