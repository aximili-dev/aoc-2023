(in-package #:aoc-2023)

(defparameter *strength* 2)

(defun parse-galaxies (line index)
  (let ((positions))
    (cl-ppcre:do-matches (s e "#" line)
      (push (cons s index) positions))
    (reverse positions)))

(defun get-empties (galaxies max-index method)
  (loop for i from 0 to max-index
	when (notany (lambda (galaxy)
		       (= (funcall method galaxy) i))
		     galaxies)
	  collect i))

(defun apply-dark-energy (galaxy empty-xs empty-ys)
  (destructuring-bind (gx . gy) galaxy
    (let ((affecting-xs (remove-if-not (lambda (x)
					 (< x gx))
				       empty-xs))
	  (affecting-ys (remove-if-not (lambda (y)
					 (< y gy))
				       empty-ys)))
      (cons (+ gx (* (- *strength* 1) (length affecting-xs)))
	    (+ gy (* (- *strength* 1) (length affecting-ys)))))))

(defun manhattan-distance (a b)
  (destructuring-bind (ax . ay) a
    (destructuring-bind (bx . by) b
      (+ (abs (- bx ax))
	 (abs (- by ay))))))

(defsolution (lines) 11 1
  (let* ((galaxies (loop for y from 0
			 for line in lines
			 append (parse-galaxies line y)))
	 (max-x (apply #'max (mapcar #'car galaxies)))
	 (max-y (apply #'max (mapcar #'cdr galaxies)))
	 (empty-xs (get-empties galaxies max-x #'car))
	 (empty-ys (get-empties galaxies max-y #'cdr))
	 (real-galaxies (mapcar (lambda (galaxy)
				  (apply-dark-energy galaxy empty-xs empty-ys))
				galaxies)))
    (reduce #'+
	    (apply #'append
		   (maplist (lambda (galaxies)
			      (destructuring-bind (galaxy . rest) galaxies
				(mapcar (lambda (other)
					  (manhattan-distance galaxy other))
					rest)))
			    real-galaxies)))))

