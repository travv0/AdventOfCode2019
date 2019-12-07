(defpackage :day-4
  (:use :cl))

(in-package :day-4)

(defun number-to-char-list (number)
  (coerce (write-to-string number) 'list))

(defun check-password-part-1 (number)
  (let ((number-char-list (number-to-char-list number)))
    (and (apply #'char-not-greaterp number-char-list)
         (has-sequential-digits-p number-char-list))))

(defun has-sequential-digits-p (number-char-list)
  (loop for (char-1 char-2) in (maplist #'identity number-char-list)
        when (eql char-1 char-2)
          return t))

(defun count-valid-passwords-in-range-part-1 (min max)
  (length (loop for i from min to max when (check-password-part-1 i) collect i)))

(defun has-exactly-two-sequential-digits-p (number-char-list)
  (loop for (char-1 char-2 char-3 char-4) in (maplist #'identity (cons nil number-char-list))
        when (and (eql char-2 char-3)
                  (not (eql char-1 char-2))
                  (not (eql char-3 char-4)))
          return t))

(defun check-password-part-2 (number)
  (let ((number-char-list (number-to-char-list number)))
    (and (apply #'char-not-greaterp number-char-list)
         (has-exactly-two-sequential-digits-p number-char-list))))

(defun count-valid-passwords-in-range-part-2 (min max)
  (length (loop for i from min to max when (check-password-part-2 i) collect i)))
