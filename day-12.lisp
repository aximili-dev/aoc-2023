(in-package #:aoc-2023)

(defun parse-broken-strings (line)
  (cl-ppcre:register-groups-bind (spring-descriptor
				  ((lambda (s)
				     (parse-number-list s :token #\,))
				   spring-groups))
      ("([.?#]+) ([0-9,]+)" line)
    (cons spring-descriptor
	  spring-groups)))

(defun parse-folded-broken-strings (line)
  (cl-ppcre:register-groups-bind (spring-descriptor
				  ((lambda (s)
				     (parse-number-list s :token #\,))
				   spring-groups))
      ("([.?#]+) ([0-9,]+)" line)
    (cons (concatenate 'string
		       spring-descriptor
		       "?"
		       spring-descriptor
		       "?"
		       spring-descriptor
		       "?"
		       spring-descriptor
		       "?"
		       spring-descriptor)
	  (append spring-groups
		  spring-groups
		  spring-groups
		  spring-groups
		  spring-groups))))

(defun chunk-descriptor (descriptor)
  (split-sequence #\. descriptor :remove-empty-subseqs t))

(defun applies-to-chunk-p (length chunk))

(defun find-first-fit (descriptor length start)
  (cl-ppcre:scan (format nil "[^#][#?]{~d}[^#]" length)
		 descriptor
		 :start start))

(defun find-combinations-single (descriptor length start)
  (labels ((rec (start)
	     (let ((pos (find-first-fit descriptor length start)))
	       (if pos
		   (1+ (rec (1+ pos)))
		   0))))
    (rec start)))

(defun legal-p (descriptor groups)
  (let ((regex ))))

(defun find-combinations-pound (descriptor groups)
  (declare (special *cache-hits* *cache-misses* *combo-cache*))
  (let ((cached (gethash (cons descriptor groups) *combo-cache*)))
    (if cached
	(progn
	  (incf *cache-hits*)
	  cached)
	(let ((res 
		(let* ((next-char (unless (= 1 (length descriptor))
				    (elt descriptor 1))))
		  (cond
		    ;; Looking at pound, expecting 0
		    ;; Illegal combo, push up 0
		    ((eq 0 (first groups))
		     0)
		    ;; Looking at pound, expecting 1, next is nil
		    ;; Legal, recurse empty tail
		    ((and (eq 1 (first groups))
			  (null next-char))
		     (find-combinations (subseq descriptor 1)
					(cdr groups)))
		    ;; Looking at pound, expecting 1, next is #
		    ;; Illegal combo, push up 0
		    ((and (eq 1 (first groups))
			  (eq #\# next-char))
		     0)
		    ;; Looking at pound, expecting 1, next is [.?]
		    ;; Legal, recurse skipping one
		    ((and (eq 1 (first groups))
			  (not (eq next-char #\#)))
		     (find-combinations (subseq descriptor 2)
					(cdr groups)))
		    ;; Looking at pound, expecting n, next is nil
		    ;; Illegal combo, push up 0
		    ((eq next-char nil)
		     0)
		    ;; Looking at pound, expecting n, next is #
		    ;; Legal, recurse with 1- group
		    ((eq next-char #\#)
		     ;; used to be pound
		     (find-combinations (subseq descriptor 1)
					(cons (1- (first groups))
					      (cdr groups))))
		    ;; Looking at pound, expecting n, next is .
		    ;; Illegal combo, push up 0
		    ((eq next-char #\.)
		     0)
		    ;; Looking at pound, expecting n, next is ?
		    ;; Try # and recurse
		    ((eq next-char #\?)
		     (find-combinations-pound (concatenate 'string
							   "#"
							   (subseq descriptor 2))
					      (cons (1- (first groups))
						    (cdr groups))))))))
	  (incf *cache-misses*)
	  (setf (gethash (cons descriptor groups)
			 *combo-cache*)
		res)))))

(defun find-combinations (descriptor groups)
  (declare (special *cache-hits* *cache-misses* *combo-cache*))
  (let ((cached (gethash (cons descriptor groups) *combo-cache*)))
    (if cached
	(progn
	  (incf *cache-hits*)
	  cached)
	(let ((res
		(cond
		  ;; No more descriptor, no more groups.
		  ;; 1 combo has been found, push it up
		  ((and (= 0 (length descriptor))
			(= 0 (length groups)))
		   1)
		  ;; No more descriptor, still some groups
		  ;; Illegal combo, push up 0
		  ((= 0 (length descriptor)) 0)
		  ;; No more groups, descriptor of [.?]
		  ;; 1 combo found, push it up
		  ((and (= 0 (length groups))
			(every (lambda (char)
				 (or (eq char #\.)
				     (eq char #\?)))
			       descriptor))
		   1)
		  ;; No more groups, descriptor has #
		  ;; Illegal combo, push up 0
		  ((and (= 0 (length groups))
			(some (lambda (char)
				(eq char #\#))
			      descriptor))
		   0)
		  ;; Descriptor and groups remain, check combos
		  (t (let ((head (elt descriptor 0))
			   (tail (subseq descriptor 1)))
		       (case head
			 ;; Dot, check tail
			 (#\. (find-combinations tail groups))
			 ;; Wildcard, check both options
			 (#\? (+ (find-combinations (concatenate 'string '(#\.) tail)
						    groups)
				 (find-combinations (concatenate 'string '(#\#) tail)
						    groups)))
			 ;; Pound, check combos
			 (#\# (find-combinations-pound descriptor groups))))))))
	  (incf *cache-misses*)
	  (setf (gethash (cons descriptor groups)
			 *combo-cache*)
		res)))))

(defsolution (lines :parser #'parse-broken-strings) 12 1
  (let ((*cache-hits* 0)
	(*cache-misses* 0)
	(*combo-cache* (make-hash-table :test #'equal)))
    (reduce #'+
	    (mapcar (lambda (line)
		      (find-combinations (car line) (cdr line)))
		    lines))))

(defparameter *cache-hits* 0)
(defparameter *cache-misses* 0)
(defparameter *combo-cache* (make-hash-table :test 'equal))

(defsolution (lines :parser #'parse-folded-broken-strings) 12 2
  (let ((*cache-hits* 0)
	(*cache-misses* 0)
	(*combo-cache* (make-hash-table :test 'equal)))
    (loop for line in lines
	  for i from 0
	  do (format t "~a ~a~%" i (find-combinations (car line) (cdr line))))
    (reduce #'+
	    (mapcar (lambda (line)
		      (find-combinations (car line) (cdr line)))
		    lines))))
