(asdf:defsystem #:aoc2019.day6
  :serial t
  :depends-on (:alexandria :split-sequence :cl-graph :cl-containers)
  :pathname "./"
  :components ((:file "day6")))
