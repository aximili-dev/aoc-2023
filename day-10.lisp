(in-package #:aoc-2023)

(defparameter *map-elements* '((#\- . :east-west)
			       (#\| . :north-south)
			       (#\L . :north-east)
			       (#\F . :south-east)
			       (#\7 . :south-west)
			       (#\J . :north-west)
			       (#\S . :animal)
			       (#\. . :ground)))

(defparameter *connectors* '(:east-west
			     :north-south
			     :north-east
			     :south-east
			     :north-west
			     :south-west))

(defparameter *connections* `((:east-west . ,(remove :north-south *connectors*))
			      (:north-south . ,(remove :east-west *connectors*))
			      (:north-east . ,(remove :south-west *connectors*))
			      (:south-east . ,(remove :north-west *connectors*))
			      (:south-west . ,(remove :north-east *connectors*))
			      (:north-west . ,(remove :south-east *connectors*))))

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

(defun mapref (coords map)
  (nth (car coords)
       (nth (cdr coords)
	    map)))

(defun add-coords (a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b))))

(defun find-next-step (cur-pos prev-pos map)
  (let* ((cur (mapref cur-pos map))
	 (deltas '((0 . 1)
		   (0 . -1)
		   (1 . 0)
		   (-1 . 0)))
	 (neighbors (mapcar (lambda (delta)
			      (add-coords cur-pos delta))
			    deltas))
	 (neighbors (remove prev-pos neighbors :test #'equal))
	 (neighbors (remove-if (lambda (node)
				 (or (minusp (car node))
				     (munusp (cdr node))))
			       neighbors))
	 (neighbor-vals (mapcar (lambda (pos)
				  (cons pos (mapref pos map)))
				neighbors)))
    (remove-if-not (lambda (neighbor)
		     (find neighbor (cdr (assoc cur *connections)))))))

(defun walk-loop (start map)
  (loop with cur = start
	with prev = nil))

(defsolution (map :parser #'parse-map-line) 10 1
  (find-animal-position map))
