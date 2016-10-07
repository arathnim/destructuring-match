(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))
(setf *print-case* :downcase)
(ql:quickload '(alexandria cl-ppcre iterate anaphora) :silent t)

(defpackage destr-match
 (:use cl alexandria iterate cl-ppcre anaphora))
(in-package destr-match)

(defparameter destr-match-clauses (make-hash-table :test #'equalp))
(defvar *list* nil)

(defmacro def-match-clause (name ll &rest rest)
	`(setf (gethash ',name destr-match-clauses) (lambda ,ll ,@rest)))

(defun parse-out-free-symbols (exp)
   (delete-duplicates
      (if (listp exp) 
          (iter (for x in exp)
                (when (and (eq x (car exp)) (gethash x destr-match-clauses)) (next-iteration))
                (aif (parse-out-free-symbols x) (appending it)))
          (when (symbolp exp) (list exp)))))

;; remember to add a list test and nil case
;; how to be decide to stop on the last sym case?
;;   test for nil list somehow, win if more than 0 or 1 iterations?
(defmacro match (exp)
	(if (stringp (car exp))
		 `(when (and *list* (eq ,(intern (string-upcase (car exp))) (car *list*)))
			     (let ((*list (cdr *list*)))
					     (match ,(cdr exp))))
		 (aif (gethash (car exp) destr-match-clauses)
			   (apply it (cdr exp))
				(with-gensyms (i)
					`(iter (for ,i on *list*)
							 (setf ,(cdr list) ,i)
							 (when (match ,(cddr exp))
								    (leave t)))))))

;; clauses
;;   switch - matches forms, takes the first to work, analogous to parmesan choice
;;   lazy, greedy - only on top on a single binding form
;;   any - causes it to match zero or more list elements
;;   single - causes it to match only one form
;;   test - means the form must match the condition described as well as the structure of the match
;;   key - same as test, except the result of the function is bound instead of the normal result
;;   on-fail - expression to be returned by the whole thing if that subform fails to match

(def-match-clause single (rest)
	`(when *list* 
			  (setf ,var (car *list*))
			  (let ((*list* (cdr *list*))) 
				     (match ,rest))))

;; general process
;;   process the match-form
;;   expand macros, warning or error on actual functions
;;   parse out the relevent symbols
;;   recursive expansion of clauses
;;   generate the full form, inside a let
(defmacro destructuring-match (exp match-form &rest body)
   (let ((match-form (macroexpand-all match-form)))
			()))
