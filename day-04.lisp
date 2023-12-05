(in-package #:aoc-2023)

(defun parse-card (line)
  (cl-ppcre:register-groups-bind ((#'parse-number-list winning-numbers my-numbers))
      ("Card +[0-9]+: +((?:\\d+ *)+)\\| +((?:\\d+ *)+)" line)
    (cons winning-numbers my-numbers)))

(defun parse-card-with-id-and-count (line)
  (cl-ppcre:register-groups-bind ((#'parse-integer card-id)
				  (#'parse-number-list winning-numbers my-numbers))
      ("Card +([0-9]+): +((?:\\d+ *)+)\\| +((?:\\d+ *)+)" line)
    (list card-id 1 winning-numbers my-numbers)))

(defun card-value (winning-numbers my-numbers)
  (let ((i (intersection winning-numbers my-numbers)))
    (if i
	(expt 2 (1- (length i)))
	0)))

(defun matching-numbers (a b)
  (length (intersection a b)))

(defsolution (cards :parser #'parse-card) 4 1
  (reduce #'+
	  (mapcar (lambda (card)
		    (card-value (car card)
				(cdr card)))
		  cards)))

(defsolution (cards :parser #'parse-card-with-id-and-count) 4 2
  (dolist (card cards)
    (destructuring-bind (card-id card-count winning-numbers my-numbers) card
      (declare (ignorable card-count))
      (let ((wins (matching-numbers winning-numbers my-numbers)))
	(dotimes (delta wins)
	  (incf (second (or (nth (+ card-id delta) cards)
			    '(0 0)))
		card-count)))))
  (reduce #'+ cards
	  :key #'second))
