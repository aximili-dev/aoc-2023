;;;; aoc-2023.lisp

(in-package #:aoc-2023)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defparameter *solutions* '())

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args)))) 

(defun input-name (day part &optional demo)
  (with-output-to-string (s)
    (format s "~2,'0d-" day)
    (if demo
	(format s "demo-~d" part)
	(format s "input"))))

(defun input-path (day part &optional demo)
  (make-pathname :directory '(:relative "inputs")
		 :name (input-name day part demo)
		 :type "txt"))

(defun solution-symb (day part)
  (symb 'aoc "-" day "-" part))

(defmacro defsolution ((file-lines &key parser) day part &body body)
  `(defun ,(solution-symb day part) (&optional demo)

     (let ((,file-lines '()))
       (handler-bind ((file-error (lambda (c)
				    (declare (ignorable c))
				    (use-value (input-path ,day (1- ,part) demo)))))
	 (with-open-file (s (input-path ,day ,part demo))
	   (do ((line (read-line s nil) (read-line s nil)))
	       ((null line))
	     (push line ,file-lines))
	   
	   (setf ,file-lines (reverse ,file-lines))

	   ,(if parser
		`(setf ,file-lines (mapcar ,parser ,file-lines)))))
       
       ,@body)))

(defmacro run-solution (day part &optional demo)
  `(funcall #',(solution-symb day part)
	    ,demo))
