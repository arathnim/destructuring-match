(proclaim '(optimize (speed 3) (safety 0) (debug 0) (space 0)))
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))
(setf *print-case* :downcase)
;; (ql:quickload '(alexandria iterate anaphora) :silent t)

(defpackage destr-match
  (:use cl alexandria iterate anaphora)
  (:shadow switch))
(in-package destr-match)

;; TODO
;;   [ ] neat switch macro
;;   [ ] neat function definition macro
;;   [ ] fill out the README with examples and documentation
;;   [ ] general atom matching, just because
;;   [ ] sublists?
;;   [ ] make special parsing for single inside test and key
;;   [ ] macroexpansion doesn't actually word, modify match
;;   [x] if the form provided to d-m has a car that is a clause, wrap it in a list
;;   [ ] make switch (list) raw forms, so everything still works
;;   [ ] modify match so clause names without arguments are bindings; may also require changes to parsing

(defparameter destr-match-clauses (make-hash-table :test #'equalp))
(defparameter matching-style 'symbol) ;; symbol, string, case-sensitive, regex, symbol-regex

(defmacro def-match-clause (name ll &rest rest)
   `(setf (gethash ',name destr-match-clauses) (lambda ,ll ,@rest)))

(defparameter walking-behavior
   `((test ,#'second)
     (key ,#'second)))

;; NOTE combine these two by appendf'ing into a list? shouldn't be too hard
(defun expand-macros (exp)
	(if (and (listp exp) (symbolp (car exp)))
		 (if (gethash (car exp) destr-match-clauses)
			  (let ((b (second (assoc (car exp) walking-behavior))))
				     (expand-macros (if b (funcall b exp) (cdr exp))))
			  (multiple-value-bind (a b) (macroexpand exp)
			     (if b a exp)))
		 exp))

(defun parse-out-free-symbols (exp)
   (delete-duplicates
      (if (listp exp)
          (if (gethash (car exp) destr-match-clauses)
              (let ((b (second (assoc (car exp) walking-behavior))))
					     (print exp)
                    (parse-out-free-symbols (if b (funcall b exp) (cdr exp))))
              (iter (for x in exp)
                    (aif (parse-out-free-symbols x) (appending it))))
          (when (symbolp exp) (list exp)))))

(defun match-test (str)
   (case matching-style
      (symbol         `(eq ',(intern (string-upcase str)) (car list)))
      (string         `(equalp  ,str (car list)))
      (case-sensitive `(string= ,str (car list)))
      (regex          `(cl-ppcre:scan ,str (car list)))
      (symbol-regex   `(cl-ppcre:scan ,str (string (car list))))
      (otherwise       (error "something went very wrong"))))

(defun generate-match-code (exp end)
   `(when (and list ,(match-test (car exp)))
         ,(if end 't
            `(let ((list (cdr list)))
                   (match ,(cdr exp))))))

(defun generate-bind-code (exp end)
   (if end
       `(when list (setf ,(car exp) list) t)
       (with-gensyms (v x w r loop succeed fail end)
         `(let ((,v (list (car list))) (,x (cdr list)) (,w nil) (,r nil))
                (tagbody
                  ,loop
                     (when (not ,x) (go ,fail))
                     (setf ,w (let ((list ,x)) (match ,(cdr exp))))
                     (when (eq ,w t) (go ,succeed))
                     (appendf ,v (list (car ,x)))
                     (setf ,x (cdr ,x))
                     (go ,loop)
                  ,fail
                     (setf ,(car exp) nil)
                     (setf ,r nil)
                     (go ,end)
                  ,succeed
                     (setf ,(car exp) ,v)
                     (setf ,r ,w)
                  ,end)
                ,r))))

(defmacro match (exp)
	(print `(match ,exp))
   (let ((end-of-chain? (not (cdr exp))))
      (if (listp (car exp))
          (aif (gethash (caar exp) destr-match-clauses)
               (funcall it (cdar exp) (cdr exp) end-of-chain?)
               (error "stuff is seriously messed up here: ~a" exp)) 
          (if (stringp (car exp))
              (generate-match-code exp end-of-chain?)
              (generate-bind-code exp end-of-chain?)))))

;; clauses
;;   switch - matches forms, takes the first to work, analogous to parmesan choice
;;   single - causes it to match only one form
;;   optional - if it matches, take that out of the list, keep matching either way,
;;     putting a single binding var at the end will break everything, as usual
;;   test - means the form must match the condition described as well as the structure of the match
;;   key - same as test, except the result of the function is bound instead of the normal result

(def-match-clause optional (forms rest end)
   (if end
       `(match ,forms)
       (with-gensyms (foo)
          `(let ((,foo (match ,(append forms rest))))
                 (if ,foo ,foo (match ,rest))))))

(def-match-clause switch (forms rest end)
   (if (not (cdr forms))
       (if end 
         `(match ,(car forms))
         `(match ,(append (car forms) rest)))
       (if end
           (with-gensyms (foo)
             `(let ((,foo (match ,(first forms))))
                    (if ,foo ,foo (match ((switch ,@(cdr forms)))))))
           (with-gensyms (foo)
             `(let ((,foo (match ,(append (car forms) rest))))
                    (if ,foo ,foo (match ,(append `((switch ,@(cdr forms))) rest))))))))

(def-match-clause single (var rest end)
   (if end
       `(progn (setf ,(car var) (car list))
               (if (not (cdr list))
                   t
                   (cdr list)))
       `(when (car list)
          (setf ,(car var) (car list))
          (let ((list (cdr list)))
                (match ,rest)))))

(def-match-clause test (var rest end)
   (if end
      `(when (and list (funcall ,(second var) list)) (setf ,(first var) list) t)
       (with-gensyms (v x w r loop succeed fail end)
         `(let ((,v (list (car list))) (,x (cdr list)) (,w nil) (,r nil))
                (tagbody
                  ,loop
                     (when (not ,x) (go ,fail))
                     (setf ,w (let ((list ,x)) (match ,rest)))
                     (when (and (eq ,w t) (funcall ,(second var) ,v)) (go ,succeed))
                     (appendf ,v (list (car ,x)))
                     (setf ,x (cdr ,x))
                     (go ,loop)
                  ,fail
                     (setf ,(car var) nil)
                     (setf ,r nil)
                     (go ,end)
                  ,succeed
                     (setf ,(car var) ,v)
                     (setf ,r ,w)
                  ,end)
                ,r))))

(def-match-clause key (var rest end)
   (if end
      `(awhen (and list (funcall ,(second var) list)) (setf ,(first var) it) t)
       (with-gensyms (v x w r f loop succeed fail end)
         `(let ((,v (list (car list))) (,x (cdr list)) (,w nil) (,r nil))
                (tagbody
                  ,loop
                     (when (not ,x) (go ,fail))
                     (setf ,w (let ((list ,x)) (match ,rest)))
                     (let ((,f (funcall ,(second var) ,v))) 
                           (when (and (eq ,w t) ,f)
                                 (setf ,(car var) ,f) 
                                 (go ,succeed)))
                     (appendf ,v (list (car ,x)))
                     (setf ,x (cdr ,x))
                     (go ,loop)
                  ,fail
                     (setf ,(car var) nil)
                     (setf ,r nil)
                     (go ,end)
                  ,succeed
                     (setf ,r ,w)
                  ,end)
                ,r))))

(defun simple-pair (list val)
   (mapcar (lambda (x) (list x val)) list))

(defmacro destructuring-match (exp match-form &rest body)
   (when (eq exp :mode)
         (setf matching-style match-form)
         (setf exp (first body))
         (setf match-form (second body))
         (setf body (cddr body)))
	(when (gethash (car match-form) destr-match-clauses)
		   (setf match-form (list match-form)))
   (let ((match-form (macroexpand `(match ,match-form)))
		   (bindings (parse-out-free-symbols match-form)))
        `(let ,(append `((list ,exp)) (simple-pair bindings nil))
            (if ,match-form
                ,@body 
                 nil))))

(defmacro match-keyword (x) `(test ,x (lambda (ll) (keyword? (car ll)))))
