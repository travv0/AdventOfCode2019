(asdf:defsystem #:intcode-interpreter
  :serial t
  :depends-on (:split-sequence :cl-arrows :alexandria)
  :pathname "./"
  :components ((:file "day9")))
