(defpackage #:day16
  (:use #:cl #:alexandria #:cl-arrows))

(in-package #:day16)

(defun create-pattern (pos base-pattern)
  (->> base-pattern
       (mapcar (curry #'make-list pos :initial-element))
       flatten
       (apply #'circular-list)
       cdr))

(defun digits (int-string)
  (map 'list
       (compose #'parse-integer #'string)
       (string-trim '(#\Newline) int-string)))

(defun apply-pattern (base-pattern digits)
  (flet ((calc-new-digit (i)
           (-<> (create-pattern i base-pattern)
                (mapcar #'* digits <>)
                (reduce #'+ <>)
                (rem <> 10)
                abs)))
    (mapcar #'calc-new-digit (iota (length digits) :start 1))))

(defun fft (num-of-phases base-pattern digits)
  (if (<= num-of-phases 0)
      digits
      (fft (1- num-of-phases) base-pattern (apply-pattern base-pattern digits))))

(defun main (&key (part 1))
  (let* ((digits (digits (read-file-into-string "input.txt"))))
    (case part
      (1 (let ((result (fft 100 '(0 1 0 -1) digits)))
           (parse-integer (map 'string #'digit-char (subseq result 0 8)))))
      (2 (error "unimplemented"))
      (otherwise (error "`part' must be either 1 or 2")))))
