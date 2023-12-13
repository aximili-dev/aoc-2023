(in-package #:aoc-2023)

(defparameter *map-elements* '((#\- . :east-west)
			       (#\| . :north-south)
			       (#\L . :north-east)
			       (#\F . :south-east)
			       (#\7 . :south-west)
			       (#\J . :north-west)
			       (#\S . :animal)
			       (#\. . :ground)))

(defun parse-map-line (line)
  (map 'list
       (lambda (char)
	 (cdr (assoc char *map-elements*)))
       line))

(defun find-animal-position (map)
  (let* ((y (position-if (lambda (line)
			   (find :animal line))
			 map))
	 (x (position :animal (nth y map))))
    (cons x y)))

(defun neighbors (cell pos)
  (let ((deltas (case cell
		  (:north-south '((0 . -1) (0 . 1)))
		  (:east-west '((1 . 0) (-1 . 0)))
		  (:north-east '((0 . -1) (1 . 0)))
		  (:south-east '((0 . 1) (1 . 0)))
		  (:south-west '((0 . 1) (-1 . 0)))
		  (:north-west '((0 . -1) (-1 . 0))))))
    (remove-if (lambda (delta)
		 (or (minusp (car delta))
		     (minusp (cdr delta))))
	       (mapcar (lambda (delta)
			 (cons (+ (car pos) (car delta))
			       (+ (cdr pos) (cdr delta))))
		       deltas))))

(defun physical-neighbors (cell-pos grid-width grid-height)
  (let ((deltas '((0 . 1)
		  (0 . -1)
		  (1 . 0)
		  (-1 . 0))))
    (remove-if (lambda (delta)
		 (or (minusp (car delta))
		     (minusp (cdr delta))
		     (>= (car delta) grid-width)
		     (>= (cdr delta) grid-height)))
	       (mapcar (lambda (delta)
			 (cons (+ (car cell-pos) (car delta))
			       (+ (cdr cell-pos) (cdr delta))))
		       deltas))))

(defsolution (map :parser #'parse-map-line) 10 1
  (let ((graph (make-hash-table :test 'equal))
	(animal-position (find-animal-position map)))
    (loop for y from 0
	  for line in map
	  do (loop for x from 0
		   for cell in line
		   do (setf (gethash (cons x y) graph)
			    (neighbors cell (cons x y)))))
    (let ((animal-neighbors '()))
      (maphash (lambda (key val)
		 (when (find animal-position val :test #'equal)
		   (push key animal-neighbors)))
	       graph)
      (setf (gethash animal-position graph) animal-neighbors))
    (loop with cur = animal-position
	  with prev = nil
	  for next = (first (remove prev (gethash cur graph) :test #'equal))
	  for i from 1
	  do (progn
	       (format t "cur: ~a prev: ~a next: ~a neighbors: ~a~%" cur prev next (gethash cur graph))
	       (setf prev cur)
	       (setf cur next))
	  until (equal cur animal-position)
	  finally (return i))))

(defparameter *cell-classes* (make-hash-table :test 'equal))

(defun classify-cell (cell-pos grid-width grid-height)
  (declare (special *cell-classes*))
  (setf (gethash cell-pos *cell-classes*) :unknown)
  (labels ((rec (pos)
	     (let ((neighbors (physical-neighbors pos)))
	       (cond
		 ;; Less than 4 neighbors means this cell is on the outside
		 ((< (length pos) 4)
		  (setf (gethash pos *cell-classes) :outside)
		  :outside)
		 ((eq (gethash pos *cell-classes*) :path)
		  :inside)))))))

;; Mark path and animal as path
;; walk path, check neighbor class
;;   if unclassed neighbors, check neighbor class
;;      if ambigious, just count
;;      if touched border, outside
;;   if all ambigious, inside
(defsolution (map :parser #'parse-map-line) 10 2
  (let ((graph (make-hash-table :test 'equal))
	(classifications (make-hash-table :test 'equal))
	(animal-position (find-animal-position map)))
    
    (loop for y from 0
	  for line in map
	  do (loop for x from 0
		   for cell in line
		   do (setf (gethash (cons x y) graph)
			    (neighbors cell (cons x y)))))
    
    (let ((animal-neighbors '()))
      (maphash (lambda (key val)
		 (when (find animal-position val :test #'equal)
		   (push key animal-neighbors)))
	       graph)
      (setf (gethash animal-position graph) animal-neighbors))
    
    (loop with cur = animal-position
	  with prev = nil
	  for next = (first (remove prev (gethash cur graph) :test #'equal))
	  for i from 1
	  do (progn
	       (setf (gethash cur classifications) :path)
	       (format t "cur: ~a prev: ~a next: ~a neighbors: ~a~%" cur prev next (gethash cur graph))
	       (setf prev cur)
	       (setf cur next))
	  until (equal cur animal-position))

    
    classifications))
