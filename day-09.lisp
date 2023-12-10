(in-package #:aoc-2023)

(defun calculate-seq-diffs (seq)
  (loop for tail on seq
	while (> (length tail) 1)
	collect (- (second tail) (first tail))))

(defun calculate-all-diffs (seq)
  (if (every #'zerop seq)
      (list seq)
      (let ((diff (calculate-seq-diffs seq)))
	(apply #'list seq (calculate-all-diffs diff)))))

(defun calculate-final-elt (diffs)
  (destructuring-bind (seq . rest) diffs
    (if (every #'zerop seq)
	0
	(+ (car (last seq))
	   (calculate-final-elt rest)))))

(defun calculate-first-elt (seq)
  (let ((diffs (calculate-all-diffs seq)))
    (labels ((rec (diffs)
	       (destructuring-bind (seq . rest) diffs
		 (if (every #'zerop seq)
		     0
		     (- (first seq)
			(rec rest))))))
      (rec diffs))))

(defsolution (seqs :parser #'parse-number-list) 9 1
  (let ((diffs (mapcar #'calculate-all-diffs seqs)))
    (reduce #'+
	    (mapcar #'calculate-final-elt diffs))))

(defsolution (seqs :parser #'parse-number-list) 9 2
  (reduce #'+ (mapcar #'calculate-first-elt seqs)))
