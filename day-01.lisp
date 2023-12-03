(in-package #:aoc-2023)

(defparameter *digit-names* '(("one" . "o1e")
			      ("two" . "t2o")
			      ("three" . "t3e")
			      ("four" . "f4r")
			      ("five" . "f5e")
			      ("six" . "s6x")
			      ("seven" . "s7n")
			      ("eight" . "e8t")
			      ("nine" . "n9e")))

(defun find-first-english-digit (line)
  (reduce (lambda (&optional a b)
	    (cond
	      ((null a) b)
	      ((null b) a)
	      (t (if (< (cdr a) (cdr b))
		     a
		     b))))
	  (remove-if (lambda (x)
		       (null (cdr x)))
		     (mapcar (lambda (cons)
			       (cons (car cons)
				     (cl-ppcre:scan (car cons) line)))
			     *digit-names*))))

(defun transform-line (line)
  (let ((cur line))
    (loop for next-digit = (find-first-english-digit cur)
	  while next-digit
	  do (setf cur (cl-ppcre:regex-replace-all (car next-digit)
						   cur
						   (cdr (assoc (car next-digit)
							       *digit-names*
							       :test #'equal)))))
    cur))

(defun extract-calibration-value (line)
  (let ((filtered (coerce (remove-if-not #'digit-char-p line) 'list)))
    (parse-integer (coerce (list (first filtered)
				 (car (last filtered)))
			   'string))))

(defsolution (lines) 1 1
  (reduce #'+
	  (mapcar #'extract-calibration-value lines)))

(defsolution (lines) 1 2
  (reduce #'+
	  (mapcar (lambda (line)
		    (extract-calibration-value (transform-line line)))
		  lines)))
