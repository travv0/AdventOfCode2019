(defpackage #:day22
  (:use #:cl
        #:alexandria
        #:pettomato-deque))

(in-package #:day22)

(defun deal-into-new-stack (deck)
  (let ((new-deck (make-instance 'deque)))
    (loop while (not (empty-p deck)) do
      (push-front new-deck (pop-front deck)))
    new-deck))

(defun cut-cards (n deck)
  (dotimes (i n)
    (push-back deck (pop-front deck)))
  deck)

(defun reverse-cut-cards (n deck)
  (dotimes (i (abs n))
    (push-front deck (pop-back deck)))
  deck)

(defun deal-with-increment (n deck)
  (let ((temp-deck (make-hash-table))
        (new-deck (make-instance 'deque))
        (count (element-count deck)))
    (dotimes (i count)
      (setf (gethash (mod (* i n) count)
                     temp-deck)
            (pop-front deck)))
    (loop for i from 0 below count do
      (push-back new-deck (gethash i temp-deck)))
    new-deck))

(defun parse-input (input)
  (let ((lines (str:lines input))
        (fns '()))
    (loop for line in lines do
      (push (cond ((string= line "deal into new stack") #'deal-into-new-stack)
                  ((starts-with-subseq "cut" line)
                   (cl-ppcre:register-groups-bind (n-str)
                       ("cut (-?\\d+)" line)
                     (let ((n (parse-integer n-str)))
                       (if (> n 0)
                           (curry #'cut-cards n)
                           (curry #'reverse-cut-cards n)))))
                  ((starts-with-subseq "deal" line)
                   (cl-ppcre:register-groups-bind (n-str)
                       ("deal with increment (\\d+)" line)
                     (curry #'deal-with-increment (parse-integer n-str)))))
            fns))
    (reverse fns)))

(defun shuffle-deck (deck fns)
  (loop with deck = deck
        for fn in fns do
          (setf deck (funcall fn deck))
        finally (return deck)))

(defun main ()
  (let ((deck (make-instance 'deque))
        (fns (parse-input (read-file-into-string "input.txt"))))
    (dotimes (i 10007)
      (push-back deck i))
    (position 2019 (deque->list (shuffle-deck deck fns)))))
