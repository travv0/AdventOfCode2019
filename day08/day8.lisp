(defpackage #:aoc2019.day8
  (:use #:cl #:alexandria))

(in-package #:aoc2019.day8)

(defun digits (int-string)
  (map 'list
       (compose #'parse-integer #'string)
       (string-trim '(#\Newline) int-string)))

(defun chunks-of (n list)
  (loop for l on list by (lambda (l) (subseq l (min n (length l))))
        collecting (subseq l 0 (min n (length l)))))

(defun make-image (width height input)
  (let* ((int-input (digits input))
         (layers (chunks-of (* width height) int-input)))
    layers))

(defun layer-with-least-of-digit (digit image)
  (loop for layer in image
        for current-count = (count-if (curry #'= digit) layer)
        with current-least-layer = nil
        and current-least-count = most-positive-fixnum
        finally (return current-least-layer)
        when (< current-count current-least-count)
          do (setf current-least-layer layer
                   current-least-count current-count)))

(defun squash-layers (image)
  (apply #'mapcar
         (lambda (&rest pixels)
           (car (remove-if (curry #'= 2) pixels)))
         image))

(defun print-final-image (width image)
  (loop for row in (chunks-of width image) do
    (loop for pixel in row do
      (format t "~a" (if (= pixel 1) "." " ")))
    (format t "~%")))

(defun main (&key (part 2))
  (let* ((input (string-trim '(#\Newline) (read-file-into-string "input.txt")))
         (image (make-image 25 6 input)))
    (case part
      (1 (let ((layer (layer-with-least-of-digit 0 image)))
           (* (count-if (curry #'= 1) layer) (count-if (curry #'= 2) layer))))
      (2 (print-final-image 25 (squash-layers image)))
      (otherwise (error "`part' must be either 1 or 2")))))
