(defpackage #:day16
  (:use #:cl #:alexandria #:cl-arrows)
  (:import-from #:incf-cl #:scan*))

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

(defun fast-fft (num-of-phases digits)
  (if (<= num-of-phases 0)
      digits
      (fast-fft (1- num-of-phases)
                (scan* (lambda (a b) (mod (+ a b) 10)) digits :initial-value 0 :from-end t))))

(defun main (&key (part 2))
  (let* ((digits (digits (read-file-into-string "input.txt"))))
    (case part
      (1 (let ((result (fft 100 '(0 1 0 -1) digits)))
           (parse-integer (map 'string #'digit-char (subseq result 0 8)))))
      (2 (let* ((digits (subseq (apply #'circular-list digits) 0 (* (length digits) 10000)))
                (offset (parse-integer
                         (map 'string
                              (compose (lambda (s) (char s 0)) #'write-to-string)
                              (subseq digits 0 7))))
                (result (fast-fft 100 (subseq digits offset))))
           (parse-integer (map 'string #'digit-char (subseq result 0 8)))))
      (otherwise (error "`part' must be either 1 or 2")))))
