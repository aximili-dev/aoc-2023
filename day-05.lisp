(in-package #:aoc-2023)

(defparameter *map-order* '("seed-to-soil"
			    "soil-to-fertilizer"
			    "fertilizer-to-water"
			    "water-to-light"
			    "light-to-temperature"
			    "temperature-to-humidity"
			    "humidity-to-location"))

(defun parse-seed-list (line)
  (parse-number-list (cl-ppcre:scan-to-strings "([0-9]+ *)+" line)))

(defun parse-seed-ranges (line)
  (let* ((unpaired (parse-seed-list line)))
    (loop for i from 0 below (/ (length unpaired) 2)
	  for n = (* 2 i)
	  collect (cons (nth n unpaired)
			(nth (1+ n) unpaired)))))

(defun parse-map-name (line)
  (multiple-value-bind (match map-name)
      (cl-ppcre:scan-to-strings "([a-z-]+) map:" line)
    (declare (ignorable match))
    (aref map-name 0)))

(defun between (n min max)
  (and (>= n min)
       (<  n max)))

(defun parse-map-line (line)
  (destructuring-bind (dest-start source-start length)
      (parse-number-list line)
    (lambda (n)
      (when (between n source-start (+ source-start length))
	(+ dest-start (- n source-start))))))

(defun parse-map (map-lines)
  (cons (parse-map-name (first map-lines))
	(mapcar #'parse-map-line
	        (subseq map-lines 1))))

(defun merge-map (map-a map-b)
  "Takes the lists directly"
  (loop for a in map-a
	append (destructuring-bind (dest-a source-a length-a) map-a
		 (destructuring-bind (dest-b source-b length-b) map-b))))

(defun solve-map (n map)
  (or (some (lambda (map-fun)
	      (funcall map-fun n))
	    (cdr map))
      n))

(defun resolve-seed-location (seed map-order map-alist)
  (loop with res = seed
	for map-name in map-order
	do (setf res (solve-map res (assoc map-name map-alist
					   :test #'equal)))
	finally (return res)))

(defun expected-last-seed-location (start length map-order map-alist)
  (+ (resolve-seed-location start map-order map-alist)
     (1- length)))

(defun half (n)
  "Splits a range length. (values l1 s2 l2)"
  (if (evenp n)
      (values (/ n 2)
	      (/ n 2)
	      (/ n 2))
      (values (/ (1- n) 2)
	      (/ (1- n) 2)
	      (/ (1+ n) 2))))

(defun split-seed-range (start length map-order map-alist)
  (if (= (resolve-seed-location (1- (+ start length)) map-order map-alist)
	 (expected-last-seed-location start length map-order map-alist))
      (list (cons start length))
      (multiple-value-bind (l1 s2 l2) (half length)
	(append (split-seed-range start
				l1
				map-order
				map-alist)
		(split-seed-range (+ start s2)
				  l2
				  map-order
				  map-alist)))))

(defsolution (lines) 5 1
  (let* ((seeds (parse-seed-list (first lines)))
	 (maps (split-sequence "" lines
			       :remove-empty-subseqs t
			       :start 2
			       :test #'equal))
	 (maps (mapcar #'parse-map maps)))
    (apply #'min 
	   (mapcar (lambda (seed)
		     (resolve-seed-location seed *map-order* maps))
		   seeds))))

(defsolution (lines) 5 2
  (let* ((seed-ranges (parse-seed-ranges (first lines)))
	 (maps (split-sequence "" lines
			       :remove-empty-subseqs t
			       :start 2
			       :test #'equal))
	 (maps (mapcar #'parse-map maps))
	 (seed-ranges (reduce #'append
			      (mapcar (lambda (seed-range)
					(split-seed-range (car seed-range)
							  (cdr seed-range)
							  *map-order*
							  maps))
				      seed-ranges))))
    (apply #'min (mapcar (lambda (seed-range)
			   (resolve-seed-location (car seed-range)
						  *map-order*
						  maps))
			 seed-ranges))))
