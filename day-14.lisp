(in-package #:aoc-2023)

(defun parse-rock-line (line)
  (loop for x from 0
	for c across line
	unless (eq c #\.)
	  collect (case c
		    (#\# (cons :cube x))
		    (#\O (cons :sphere x)))))

(defmacro with-el ((x y) el &body body)
  `(destructuring-bind (type . (,x . ,y)) ,el
     (declare (ignorable type ,x ,y))
     ,@body))

(defmacro x (el)
  `(or (cadr ,el)
       -1))

(defmacro y (el)
  `(or (cddr ,el)
       -1))

(defun cubep (el)
  (eq :cube (car el)))

(defun spherep (el)
  (eq :sphere (car el)))

(defmacro alignedp (a b dir)
  `(ecase ,dir
     (:north (= (x ,a) (x ,b)))
     (:south (= (x ,a) (x ,b)))
     (:east  (= (y ,a) (y ,b)))
     (:west  (= (y ,a) (y ,b)))))

(defmacro beforep (a b dir)
  `(ecase ,dir
     (:north (< (y ,a) (y ,b)))
     (:south (> (y ,a) (y ,b)))
     (:east  (> (x ,a) (x ,b)))
     (:west  (< (x ,a) (x ,b)))))

(defmacro afterp (a b dir)
  `(beforep ,b ,a ,dir))

(defun find-closest-cube (sphere map &optional (dir :north))
  (let* ((cubes (remove-if-not (lambda (el)
				 (and (cubep el)
				      (alignedp el sphere dir)
				      (beforep el sphere dir)))
			       map)))
    (ecase dir
      (:north (car (last cubes)))
      (:west (car (last cubes)))
      (:south (first cubes))
      (:east (first cubes)))))

(defun find-spheres-before (sphere map &optional (dir :north))
  (let* ((closest-cube (find-closest-cube sphere map dir))
	 (spheres (remove-if-not (lambda (el)
				   (and (spherep el)
					(alignedp el sphere dir)
					(if closest-cube
					    (afterp el closest-cube dir)
					    t)
					(beforep el sphere dir)))
				 map)))
    spheres))

(defun fall-distance (sphere map &optional (dir :north) max)
  (let ((cube (find-closest-cube sphere map dir))
	(spheres (find-spheres-before sphere map dir)))
    (ecase dir
      (:north
       (- (y sphere)			; With no blockers, fall y
	  (1+ (y cube))			; A cube reduces it by y + 1
	  (length spheres)))
      (:south
       (- (- max (y sphere) 1)
	  (if cube
	      (- max (y cube))
	      0)
	  (length spheres)))
      (:west
       (- (x sphere)
	  (1+ (x cube))
	  (length spheres)))
      (:east
       (- (- max (x sphere) 1)
	  (if cube
	      (- max (x cube))
	      0)
	  (length spheres))))))       

(defun new-pos (sphere map &optional (dir :north) max)
  (let ((distance (fall-distance sphere map dir max)))
    (cons :sphere
	  (cons (case dir
		  (:east (+ (x sphere) distance))
		  (:west (- (x sphere) distance))
		  (t (x sphere)))
		(case dir
		  (:north (- (y sphere) distance))
		  (:south (+ (y sphere) distance))
		  (t (y sphere)))))))

(defun weight (sphere max)
  (- max (y sphere)))

(defun parse-map (parsed-lines)
  (loop for y from 0
	for line in parsed-lines
	append (mapcar (lambda (el)
			 (cons (car el)
			       (cons (cdr el)
				     y)))
		       line)))

(defsolution (lines :parser #'parse-rock-line) 14 1
  (let ((map (parse-map lines)))
    (reduce #'+
	    (mapcar (lambda (sphere)
		      (weight (new-pos sphere map) (length lines)))
		    (remove-if-not #'spherep map)))))

(defun new-map (map dir max)
  (mapcar (lambda (el)
	    (if (spherep el)
		(new-pos el map dir max)
		el))
	  map))

(defun map-weight (map max)
  (reduce #'+
	  (mapcar (lambda (sphere)
		    (weight (sphere))))))

(defun apply-cycle (cycle map max)
  (loop with cur = map
	for dir in cycle
	do (format t "  ~a: ~a~%" dir (weight ))
	do (setf cur (new-map cur dir max))
	finally (return (sort cur #'rock<))))

(defun rock< (a b)
  (cond
    ((and (cubep a) (spherep b)) t)
    ((and (spherep a) (cubep b)) nil)
    ((< (x a) (x b)) t)
    ((> (x a) (x b)) nil)
    ((< (y a) (y b)) t)
    (t nil)))

(defsolution (lines :parser #'parse-rock-line) 14 2
  (let ((map (parse-map lines))
	(cycle '(:north :west :south :east))
	(iteration-map (make-hash-table :test 'equal)))
    (loop with cur = map
	  for i from 0
	  do (progn
	       (format t "i: ~d - weight: ~d~%"
		       i
		       (reduce #'+
			     (mapcar (lambda (sphere)
				       (weight sphere (length lines)))
				     (remove-if-not #'spherep cur))))
	       (setf cur (apply-cycle cycle cur (length lines)))))
    iteration-map))
