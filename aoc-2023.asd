;;;; aoc-2023.asd

(asdf:defsystem #:aoc-2023
  :description "Describe aoc-2023 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "aoc-2023")
	       (:file "day-01.lisp")))
