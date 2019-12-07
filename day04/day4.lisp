(defpackage :day-4
  (:use :cl))

(in-package :day-4)

(defun check-password (number)
  (let ((number-char-list (coerce (write-to-string number) 'list)))
    (and (apply #'char-not-greaterp number-char-list)
         (has-sequential-digits-p number-char-list))))

(defun has-sequential-digits-p (number-char-list)
  (loop for (char-1 char-2) in (maplist #'identity number-char-list)
        when (eql char-1 char-2)
          return t))

(defun count-valid-passwords-in-range (min max)
  (length (loop for i from min to max when (check-password i) collect i)))
