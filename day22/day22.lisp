(defpackage #:aoc2019.day22
  (:use #:cl
        #:alexandria))

(in-package #:aoc2019.day22)

(defun deal-into-new-stack ()
  (list -1 -1))

(defun cut-cards (n)
  (list 1 (- n)))

(defun deal-with-increment (n)
  (list n 0))

(defun parse-input (input)
  (let ((lines (str:lines input))
        (fns '()))
    (loop for line in lines do
      (push (cond ((string= line "deal into new stack") #'deal-into-new-stack)
                  ((starts-with-subseq "cut" line)
                   (cl-ppcre:register-groups-bind (n-str)
                       ("cut (-?\\d+)" line)
                     (when n-str
                       (let ((n (parse-integer n-str)))
                         (curry #'cut-cards n)))))
                  ((starts-with-subseq "deal" line)
                   (cl-ppcre:register-groups-bind (n-str)
                       ("deal with increment (\\d+)" line)
                     (when n-str
                       (curry #'deal-with-increment (parse-integer n-str))))))
            fns))
    (reverse fns)))

(defun shuffle-deck (deck-size fns)
  (loop with a = 1 and b = 0
        for fn in fns do
          (destructuring-bind (la lb) (funcall fn)
            (setf a (mod (* la a) deck-size)
                  b (mod (+ (* la b) lb) deck-size)))
        finally (return (list a b))))

(defun run-many (a b e len)
  (cond ((= e 1) (list a b))
        ((evenp e) (run-many (mod (* a a) len)
                             (mod (+ (* a b) b) len)
                             (/ e 2)
                             len))
        (t (destructuring-bind (c d)
               (run-many a b (1- e) len)
             (list
              (mod (* a c) len)
              (mod (+ (* a d) b) len))))))

(defun egcd (a b)
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r)))
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s)))
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u)))
       (q nil))
      ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))
    (setq q (floor (/ (cdr r) (car r))))))

(defun invmod (a m)
  (multiple-value-bind (r s k) (egcd a m)
    (declare (ignore k))
    (unless (= 1 r) (error "invmod: Values ~a and ~a are not coprimes." a m))
    s))

(defun main (&key (part 2))
  (let ((fns (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (let ((deck-size 10007)
               (card 2019))
           (destructuring-bind (a b) (shuffle-deck deck-size fns)
             (mod (+ (* a card) b) deck-size))))
      (2 (let ((deck-size 119315717514047)
               (card 2020)
               (shuffle-times 101741582076661))
           (destructuring-bind (a b) (apply #'run-many
                                            (append (shuffle-deck deck-size fns)
                                                    (list shuffle-times deck-size)))
             (mod (* (- card b) (invmod a deck-size)) deck-size))))
      (otherwise (error "`part' must be either 1 or 2")))))
