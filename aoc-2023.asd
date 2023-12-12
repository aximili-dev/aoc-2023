;;;; aoc-2023.asd

(asdf:defsystem #:aoc-2023
  :description "Describe aoc-2023 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:cl-ppcre
	       :cl-utilities)
  :serial t
  :components ((:file "package")
               (:file "aoc-2023")
	       (:file "day-01")
	       (:file "day-02")
	       (:file "day-03")
	       (:file "day-04")
	       (:file "day-05")
	       (:file "day-06")
	       (:file "day-07")
	       (:file "day-08")
	       (:file "day-09")
	       (:file "day-10")
	       (:file "day-11")))
