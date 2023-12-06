(in-package #:aoc-2023)

(defun parse-boat-race-line (line)
  (cl-ppcre:register-groups-bind (name (#'parse-number-list values))
      ("(\\w+): +((?:\\d+ *)+)" line)
    (cons (intern (format nil "~:@(~a~)" name) :keyword)
	  values)))

(defun parse-kerned-number-list (string)
  (parse-integer (apply #'concatenate
			'string
			(split-sequence #\Space
					string
					:remove-empty-subseqs t))))

(defun parse-kerned-race-line (line)
  (cl-ppcre:register-groups-bind (name (#'parse-kerned-number-list number))
      ("(\\w+): +((?:\\d+ *)+)" line)
    (cons (intern (format nil "~:@(~a~)" name) :keyword)
	  number)))

(defun calculate-race-distance (press-time run-time)
  (* run-time press-time))

(defun calculate-distances (race-length)
  (loop for speed from 0 to race-length
	for time = (- race-length speed)
	collect (* speed time)))

(defun winning-presses (race-length record)
  (remove-if-not (lambda (distance)
		   (> distance record))
		 (calculate-distances race-length)))

(defun n-winning-presses (race-length record)
  (length (winning-presses race-length record)))

(defsolution (lines :parser #'parse-boat-race-line) 6 1
  (let ((times (cdr (assoc :time lines)))
	(records (cdr (assoc :distance lines))))
    (reduce #'*
	    (mapcar #'n-winning-presses times records))))

(defsolution (lines :parser #'parse-kerned-race-line) 6 2
  (let ((time (cdr (assoc :time lines)))
	(record (cdr (assoc :distance lines))))
    (length (winning-presses time record))))
