(in-package #:aoc-2023)

(defun parse-instructions (line)
  (loop for c across line
	collect (case c
		  (#\R :r)
		  (#\L :l))))

(defun parse-node (line)
  (cl-ppcre:register-groups-bind ((#'make-keyword node-id left-node right-node))
      ("(\\w+) = \\((\\w+), (\\w+)\\)" line)
    (cons node-id
	  (cons left-node right-node))))

(defun parse-ghost-node (line)
  (cl-ppcre:register-groups-bind ((#'make-keyword a1 a2 a3 l1 l2 l3 r1 r2 r3))
      ("(.)(.)(.) = \\((.)(.)(.), (.)(.)(.)\\)" line)
    (cons (list a3 a2 a1)
	  (cons (list l3 l2 l1)
		(list r3 r2 r1)))))

(defparameter *cache* '())

(defun follow-instructions (instructions start nodes)
  (or (cdr (assoc start *cache* :test #'equal))
      (loop for i in instructions
	    with cur = start
	    do (setf cur (if (eq :l i)
			     (cadr (assoc cur nodes :test #'equal))
			     (cddr (assoc cur nodes :test #'equal))))
	    finally (push (cons start cur) *cache*)
	    finally (return cur))))

(defun find-length (instructions start nodes)
  (loop for n from 1
	with cur = start
	do (setf cur (follow-instructions instructions cur nodes))
	until (eq (first cur) :z)
	finally (return (* n (length instructions)))))

(defsolution (lines) 8 1
  (let ((instructions (parse-instructions (first lines)))
	(nodes (mapcar #'parse-node (subseq lines 2))))
    (format t "Instructions: ~a~%" instructions)
    (loop for n from 1
	  with cur = :aaa
	  do (setf cur (follow-instructions instructions cur nodes))
	  until (eq cur :zzz)
	  finally (return (* n (length instructions))))))

(defsolution (lines) 8 2
  (let* ((instructions (parse-instructions (first lines)))
	 (nodes (mapcar #'parse-ghost-node (subseq lines 2)))
	 (starts (remove-if-not (lambda (node)
				    (eq :a (first node)))
				  (mapcar #'car nodes))))
    (apply #'lcm
	   (mapcar (lambda (start)
		     (find-length instructions start nodes))
		   starts))))
