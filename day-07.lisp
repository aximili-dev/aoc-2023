(in-package #:aoc-2023)

(defparameter *jokerp* nil)

(defun card-value (card)
  (declare (special *jokerp*))
  (case card
    (:A 14)
    (:K 13)
    (:Q 12)
    (:J 1)
    (:T 10)
    (t card)))

(defun parse-card (string)
  (if (digit-char-p (elt string 0))
      (parse-integer string)
      (make-keyword string)))

(defun parse-hand-and-bid (line)
  (cl-ppcre:register-groups-bind ((#'parse-card c1 c2 c3 c4 c5) (#'parse-integer bid))
      ("(\\w)(\\w)(\\w)(\\w)(\\w) (\\d+)" line)
    (cons (list c1 c2 c3 c4 c5)
	  bid)))

(defun count-hand-cards (hand)
  (let ((counts '()))
    (map 'list
	 (lambda (card)
	   (if (assoc card counts)
	       (incf (cdr (assoc card counts)))
	       (push (cons card 1) counts)))
	 hand)
    counts))

(defmacro def-hand-predicate ((name hand counts) &body body)
  `(defun ,name (,hand)
     (let ((,counts (count-hand-cards ,hand)))
       ,@body)))

(defmacro count-check (n-cards &optional (n-times 1))
  `(= ,n-times
      (count-if (lambda (card-count)
		  (= (cdr card-count) ,n-cards))
		counts)))

(defmacro joker-check (n counts)
  `(= (or (cdr (assoc :j ,counts))
	  0)
      ,n))

(def-hand-predicate (five-of-a-kind-p hand counts)
  (or (count-check 5)
      (when *jokerp*
	(let ((*jokerp* nil)
	      (hand (remove :j hand)))
	  (or (and (joker-check 1 counts)
		   (four-of-a-kind-p hand))
	      (and (joker-check 2 counts)
		   (three-of-a-kind-p hand))
	      (and (joker-check 3 counts)
		   (pair-p hand))
	      (joker-check 4 counts))))))

(def-hand-predicate (four-of-a-kind-p hand counts)
  (or (count-check 4)
      (when *jokerp*
	(let ((*jokerp* nil)
	      (hand (remove :j hand)))
	  (or (and (joker-check 1 counts)
		   (three-of-a-kind-p hand))
	      (and (joker-check 2 counts)
		   (pair-p hand))
	      (and (joker-check 3 counts)))))))

(def-hand-predicate (full-house-p hand counts)
  (or (and (count-check 3)
	   (count-check 2))
      (when *jokerp*
	(let ((*jokerp* nil)
	      (hand (remove :j hand)))
	  (or (and (joker-check 1 counts)
		   (two-pair-p hand)))))))

(def-hand-predicate (three-of-a-kind-p hand counts)
  (or (count-check 3)
      (when *jokerp*
	(let ((*jokerp* nil)
	      (hand (remove :j hand)))
	  (or (and (joker-check 1 counts)
		   (pair-p hand))
	      (joker-check 2 counts))))))

(def-hand-predicate (two-pair-p hand counts)
  (or (count-check 2 2)))

(def-hand-predicate (pair-p hand counts)
  (or (count-check 2)
      (when *jokerp*
	(let ((*jokerp* nil))
	  (joker-check 1 counts)))))

(defun hand-value (hand)
  (cond
    ((five-of-a-kind-p hand) 6)
    ((four-of-a-kind-p hand) 5)
    ((full-house-p hand) 4)
    ((three-of-a-kind-p hand) 3)
    ((two-pair-p hand) 2)
    ((pair-p hand) 1)
    (t 0)))

(defun compare-same-hand (a b)
  (cond
    ((< (card-value (first a)) (card-value (first b))) t)
    ((> (card-value (first a)) (card-value (first b))) nil)
    (t (compare-same-hand (cdr a) (cdr b)))))

(defun compare-hand (a b)
  (let ((a-val (hand-value a))
	(b-val (hand-value b)))
    (cond
      ((< a-val b-val) t)
      ((> a-val b-val) nil)
      (t (compare-same-hand a b)))))

(defsolution (hands-and-bids :parser #'parse-hand-and-bid) 7 1
  (loop for hand-and-bid in (sort hands-and-bids
				  #'compare-hand
				  :key #'car)
	for i from 1
	sum (* (cdr hand-and-bid) i)))

(defsolution (hands-and-bids :parser #'parse-hand-and-bid) 7 2
  (let ((*jokerp* t))
    (loop for hand-and-bid in (sort hands-and-bids
				    #'compare-hand
				    :key #'car)
	  for i from 1
	  sum (* (cdr hand-and-bid) i))))
