(in-package #:aoc-2023)

(defun parse-mirror-line (line)
  (coerce line 'list))

(defun palindromep (sequence)
  (when (evenp (length sequence))
    (let* ((mid (/ (length sequence) 2))
	   (first (subseq sequence 0 (floor mid)))
	   (second (subseq sequence (ceiling mid))))
      (equal first (reverse second)))))

(defun transpose-map (map)
  (apply #'mapcar
	 (lambda (&rest chars)
	   (apply #'list chars))
	 map))

(defun find-reflection-line (map)
  (let ((first-palindrome (position t (maplist #'palindromep map))))
    (when first-palindrome
      (+ first-palindrome
	 (/ (- (length map)
	       first-palindrome)
	    2)))))

(defun map-value (map)
  (let ((hn (find-reflection-line map))
	(hr (find-reflection-line (reverse map)))
	(vn (find-reflection-line (transpose-map map)))
	(vr (find-reflection-line (reverse (transpose-map map)))))
    (cond
      (hn (* 100 hn))
      (hr (* 100 (- (length map) hr)))
      (vn vn)
      (vr (- (length (transpose-map map)) vr)))))

(defun map-value-all (map)
  (let ((hn (find-reflection-line map))
	(hr (find-reflection-line (reverse map)))
	(vn (find-reflection-line (transpose-map map)))
	(vr (find-reflection-line (reverse (transpose-map map)))))
    (remove nil (list (when hn (* 100 hn))
		      (when hr (* 100 (- (length map) hr)))
		      (when vn vn)
		      (when vr (- (length (transpose-map map)) vr))))))

(defun smudge-map (map row col)
  (let* ((map (mapcar #'copy-list map))
	 (current (nth col
		       (nth row map))))
    (setf (nth col
	       (nth row map))
	  (if (eq current #\.)
	      #\#
	      #\.))
    map))

(defun map-value-with-smudge (map)
  (loop for y from 0
	for row in map
	append (loop for x from 0
		     for col in row
		     append (map-value-all (smudge-map map y x)))))

(defsolution (lines :parser #'parse-mirror-line) 13 1
  (let* ((maps (split-sequence nil lines)))
    (reduce #'+
	    (mapcar #'map-value maps))))

(defsolution (lines :parser #'parse-mirror-line) 13 2
  (let* ((maps (split-sequence nil lines))
	 (original-values (mapcar #'map-value maps))
	 (smudge-values (mapcar #'map-value-with-smudge maps)))
    (mapcar (lambda (original new)
	      (find-if-not (lambda (new)
			     (or (null new)
				 (eq new original)))
			   new))
	    original-values
	    smudge-values)))
