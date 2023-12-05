(in-package #:aoc-2023)

(defun number-rect (number line line-pos-start line-pos-end)
  (let* ((x (max 0 (1- line-pos-start)))
	 (y (max 0 (1- line)))
	 (width (+ (- line-pos-start x) (- line-pos-end line-pos-start) 1))
	 (height (+ (- line y) 2)))
    (list number
	  (list x y width height))))

(defun symbol-point (point line line-pos)
  (let ((x (+ line-pos 0.5))
	(y (+ line 0.5)))
    (list point
	  (list x y))))

(defun point-in-rect-p (point rect)
  (destructuring-bind (x y width height) rect
    (destructuring-bind (px py) point
      (and (>= px x)
	   (>= py y)
	   (<= px (+ x width))
	   (<= py (+ y height))))))

(defun part-number-p (rect points)
  (some (lambda (point)
	  (point-in-rect-p (second point) rect))
	points))

(defun part-numbers (numbers points)
  (remove-if-not (lambda (number)
		   (part-number-p (second number) points))
		 numbers))

(defun gear-p (symbol numbers)
  (let ((adyacent-numbers (remove-if-not (lambda (number)
					   (point-in-rect-p (second symbol)
							    (second number)))
					 numbers)))
    (and (string= (first symbol) "*")
	 (= (length adyacent-numbers) 2)
	 (reduce #'* adyacent-numbers
		 :key #'first))))

(defun find-next-number-or-symbol (line start)
  "Returns values (number symbol match-start match-end)"
  (multiple-value-bind (match-start match-end)
      (cl-ppcre:scan "\\d+|[^0-9.]" line :start start)
    (when match-start
      (if (digit-char-p (aref line match-start))
	  (values (parse-integer line
				 :start match-start
				 :end match-end)
		  nil
		  match-start
		  match-end)
	  (values nil
		  (subseq line match-start match-end)
		  match-start
		  match-end)))))

(defmacro with-numbers-and-symbols ((numbers symbols lines) &body body)
  `(let ((,numbers '())
	 (,symbols '()))
     (loop for i from 0
	   for line in ,lines
	   do (loop with regex-start = 0
		    while (multiple-value-bind (number symbol start end)
			      (find-next-number-or-symbol line regex-start)
			    (cond
			      (number (push (number-rect number i start end) numbers))
			      (symbol (push (symbol-point symbol i start) symbols)))
			    (setf regex-start end))))
     ,@body))

(defsolution (lines) 3 1
  (with-numbers-and-symbols (numbers symbols lines)
    (reduce #'+ (part-numbers numbers symbols)
	    :key #'first)))

(defsolution (lines) 3 2
  (with-numbers-and-symbols (numbers symbols lines)
    (reduce (lambda (a b)
	      (+ (or a 0)
		 (or b 0)))
	    (mapcar (lambda (symbol)
		      (gear-p symbol numbers))
		    symbols))))
