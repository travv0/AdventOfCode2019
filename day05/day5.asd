(asdf:defsystem #:day-5
  :serial t
  :depends-on (:split-sequence :cl-arrows :alexandria)
  :pathname "./"
  :components ((:file "day5")))