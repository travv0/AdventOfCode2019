(defpackage #:day12
  (:use #:cl #:alexandria #:cl-arrows #:lparallel)
  (:import-from #:fset #:@ #:with #:lookup #:empty-map #:equal?)
  (:shadowing-import-from #:fset
                          ;; Shadowed type/constructor names
                          #:set #:map
                          ;; Shadowed set operations
                          #:union #:intersection #:set-difference #:complement
                          ;; Shadowed sequence operations
                          #:first #:last #:subseq #:reverse #:sort #:stable-sort
                          #:reduce
                          #:find #:find-if #:find-if-not
                          #:count #:count-if #:count-if-not
                          #:position #:position-if #:position-if-not
                          #:remove #:remove-if #:remove-if-not
                          #:substitute #:substitute-if #:substitute-if-not
                          #:some #:every #:notany #:notevery))

(in-package #:day12)

(unless *kernel*
  (setf *kernel* (lparallel:make-kernel 4)))

(defun make-moon (x y z)
  (map (:x (map (:pos x) (:vel 0)))
       (:y (map (:pos y) (:vel 0)))
       (:z (map (:pos z) (:vel 0)))))

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

(defun update (map key fn)
  (let ((val (@ map key)))
    (with map key (funcall fn val))))

(defun apply-gravity (moon-axis other-moon-axis)
  (let ((pos (@ moon-axis :pos))
        (other-pos (@ other-moon-axis :pos)))
    (update moon-axis :vel
            (cond ((> pos other-pos) #'1-)
                  ((< pos other-pos) #'1+)
                  (t #'identity)))))

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
  (update moon-axis :pos (lambda (pos) (+ pos (@ moon-axis :vel)))))

(defun step-once (moon-axes)
  (->> moon-axes
       simulate-gravity
       (mapcar #'apply-velocity)))

(defun run-simulation (moons num-of-steps)
  (labels ((f (xs ys zs n)
             (if (> n 0)
                 (f (step-once xs) (step-once ys) (step-once zs) (1- n))
                 (mapcar (lambda (x y z)
                           (-> (empty-map)
                               (with :x x)
                               (with :y y)
                               (with :z z)))
                         xs ys zs))))
    (f (mapcar (lambda (moon) (@ moon :x)) moons)
       (mapcar (lambda (moon) (@ moon :y)) moons)
       (mapcar (lambda (moon) (@ moon :z)) moons)
       num-of-steps)))

(defun get-potential-energy (moon)
  (let ((x (@ (@ moon :x) :pos))
        (y (@ (@ moon :y) :pos))
        (z (@ (@ moon :z) :pos)))
    (+ (abs x) (abs y) (abs z))))

(defun get-kinetic-energy (moon)
  (let ((x (@ (@ moon :x) :vel))
        (y (@ (@ moon :y) :vel))
        (z (@ (@ moon :z) :vel)))
    (+ (abs x) (abs y) (abs z))))

(defun find-steps-until-repeated-state (moons)
  (labels ((f (moon-axes &optional initial-state (num-of-steps 0))
             (if (equal? moon-axes initial-state)
                 num-of-steps
                 (f (step-once moon-axes)
                    (or initial-state moon-axes)
                    (1+ num-of-steps)))))
    (let ((xs (mapcar (lambda (moon) (@ moon :x)) moons))
          (ys (mapcar (lambda (moon) (@ moon :y)) moons))
          (zs (mapcar (lambda (moon) (@ moon :z)) moons)))
      (reduce #'lcm (pmapcar #'f (list xs ys zs))))))

(defun main (&key (part 2))
  (let ((moons (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (-<> moons
              (run-simulation <> 1000)
              (mapcar (lambda (moon) (* (get-potential-energy moon)
                                        (get-kinetic-energy moon)))
                      <>)
              (reduce #'+ <>)))
      (2 (find-steps-until-repeated-state moons))
      (otherwise (error "`part' must be either 1 or 2")))))
