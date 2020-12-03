(defpackage :aoc2019.day-4
  (:use :cl))

(in-package :aoc2019.day-4)

(defun number-to-char-list (number)
  (coerce (write-to-string number) 'list))

(defun count-valid-passwords-in-range (sequential-digit-check-func min max)
  (loop for i from min to max
        when (check-password sequential-digit-check-func i)
          count i))

(defun check-password (sequential-digit-check-func number)
  (let ((number-char-list (number-to-char-list number)))
    (and (apply #'char-not-greaterp number-char-list)
         (funcall sequential-digit-check-func number-char-list))))

(defun has-sequential-digits-p (number-char-list)
  (loop for (char-1 char-2) in (maplist #'identity number-char-list)
          thereis (eql char-1 char-2)))

(defun has-exactly-two-sequential-digits-p (number-char-list)
  (loop for (char-1 char-2 char-3 char-4) in (maplist #'identity (cons nil number-char-list))
          thereis (and (eql char-2 char-3)
                       (not (eql char-1 char-2))
                       (not (eql char-3 char-4)))))

(defun main (&key (part 2) (min 128392) (max 643281))
  (cond ((= part 1) (count-valid-passwords-in-range #'has-sequential-digits-p min max))
        ((= part 2) (count-valid-passwords-in-range #'has-exactly-two-sequential-digits-p min max))
        (t (error "`part' must be either 1 or 2"))))
