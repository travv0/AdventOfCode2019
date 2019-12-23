(defpackage #:day22
  (:use #:cl
        #:alexandria))

(in-package #:day22)

(defun deal-into-new-stack (deck-size card)
  (mod (- -1 card) deck-size))

(defun cut-cards (n deck-size card)
  (mod (- card n) deck-size))

(defun deal-with-increment (n deck-size card)
  (mod (* card n) deck-size))

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

(defun shuffle-deck (deck-size card fns)
  (loop with card = card
        for fn in fns do
          (setf card (funcall fn deck-size card))
        finally (return card)))

(defun main ()
  (let ((fns (parse-input (read-file-into-string "input.txt"))))
    (shuffle-deck 10007 2019 fns)))
