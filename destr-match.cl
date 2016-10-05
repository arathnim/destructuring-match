(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))
(setf *print-case* :downcase)
(ql:quickload '(alexandria cl-ppcre iterate anaphora) :silent t)

(defpackage destr-match
 (:use cl alexandria iterate cl-ppcre anaphora))
(in-package destr-match)

(defparameter destr-match-clauses (make-hash-table :test #'equalp))

(defun parse-out-free-symbols (exp)
   (delete-duplicates
      (if (listp exp) 
          (iter (for x in exp)
                (when (and (eq x (car exp)) (gethash x destr-match-clauses)) (next-iteration))
                (aif (parse-out-free-symbols x) (appending it)))
          (when (symbolp exp) (list exp)))))

;; clauses
;;   switch - matches forms, takes the first to work, analogous to parmesan choice
;;   lazy, greedy - only on top on a single binding form
;;   any - causes it to match zero or more list elements
;;   single - causes it to match only one form
;;   test - means the form must match the condition described as well as the structure of the match
;;   key - same as test, except the result of the lambda is returned instead of the actual binding
;;   when - same as test, except the result of the provided expression is returned
;;   on-fail - expression to be returned by the whole thing if that subform fails to match

;; general process
;;   process the match-form
;;   expand macros, warning or error on actual functions
;;   parse out the relevent symbols
;;   recursive expansion of clauses
;;   generate the full form, inside a let
(defmacro destructuring-match (exp match-form &rest body)
   )
