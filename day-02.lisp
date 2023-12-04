(in-package #:aoc-2023)

(defparameter *max-cubes* '((:red . 12)
			    (:green . 13)
			    (:blue . 14)))

(defun parse-game-round (round)
  (let ((parsed-round '()))
    (macrolet ((parse-color (color)
		 `(cl-ppcre:register-groups-bind ((#'parse-integer n))
		      ((format nil "([0-9]+) ~(~a~)" ,color) round)
		    (if n
			(push (cons ,color n) parsed-round)))))
      (parse-color :green)
      (parse-color :blue)
      (parse-color :red)

      parsed-round)))

(defun parse-game (line)
  (let* ((game-id (cl-ppcre:register-groups-bind ((#'parse-integer id))
		      ("^Game ([0-9]+)" line)
		    id))
	 (game-list (mapcar #'parse-game-round (split-sequence #\; line))))
    (list game-id game-list)))

(defun legal-game-round-p (game-round max)
  (let ((legal-colors (mapcar (lambda (max)
				(destructuring-bind (color . max-value) max
				  (if (assoc color game-round)
				      (<= (cdr (assoc color game-round))
					  max-value)
				      t)))
			      max)))
    (reduce (lambda (a b) (and a b))
	    legal-colors)))

(defun legal-game-p (game max)
  (let ((legal-rounds (mapcar (lambda (round)
				(legal-game-round-p round max))
			      game)))
    (reduce (lambda (a b) (and a b))
	    legal-rounds)))

(defun round-max (round-a round-b)
  (mapcar (lambda (color)
	    (cons color
		  (max (or (cdr (assoc color round-a)) 0)
		       (or (cdr (assoc color round-b)) 0))))
	  (list :red :green :blue)))

(defun round-power (round)
  (reduce #'* round
	  :key #'cdr))

(defsolution (games :parser #'parse-game) 2 1
  (reduce #'+
	  (mapcar (lambda (game)
		    (destructuring-bind (game-id game-rounds) game
		      (if (legal-game-p game-rounds *max-cubes*)
			  game-id
			  0)))
		  games)))

(defsolution (games :parser #'parse-game) 2 2
  (reduce #'+
	  (mapcar #'round-power
		  (mapcar (lambda (game)
			    (reduce #'round-max (second game)))
			  games))))
