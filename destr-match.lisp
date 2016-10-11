(proclaim '(optimize (speed 3) (safety 0) (debug 0) (space 3)))
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))
(setf *print-case* :downcase)
;; (ql:quickload '(alexandria iterate anaphora) :silent t)

(defpackage destr-match
 (:use cl alexandria iterate anaphora))
(in-package destr-match)

;; TODO
;;   [ ] finish off matching clauses
;;   [ ] add neat switch macro
;;   [x] key argument for matching strings instead of symbols
;;   [x]   another for case sensitivity? 
;;   [ ]   more key args
;;   [x] test everything
;;   [ ] fill out the README with examples and documentation
;;   [x] optimize binding code with tagbody and setf
;;   [ ] figure out some way to condense all that repeated bind code

(defparameter destr-match-clauses (make-hash-table :test #'equalp))
(defparameter case-sensitive? nil)
(defparameter matching-style 'symbols) ;; symbols, strings, or both

(defmacro def-match-clause (name ll &rest rest)
   `(setf (gethash ',name destr-match-clauses) (lambda ,ll ,@rest)))

(defparameter walking-behavior
   `((test ,#'second)))

(defun parse-out-free-symbols (exp)
   (delete-duplicates
      (if (listp exp) 
          (if (gethash (car exp) destr-match-clauses)
              (let ((b (second (assoc (car exp) walking-behavior))))
                    (parse-out-free-symbols (if b (funcall b exp) (cdr exp))))
              (iter (for x in exp)
                    (aif (parse-out-free-symbols x) (appending it))))
          (when (symbolp exp) (list exp)))))

(defun match-test (str)
   (case matching-style
      (symbols `(eq ',(intern (string-upcase str)) (car list)))
      (strings 
         (if case-sensitive? 
             `(string= ,str (car list))
             `(equalp  ,str (car list))))
      (both
         `(if (symbolp (car list))
              (eq ',(intern (string-upcase str)) (car list))
              ,(if case-sensitive? 
                   `(string= ,str (car list))
                   `(equalp  ,str (car list)))))))

(defun generate-match-code (exp end)
   `(when (and list ,(match-test (car exp)))
          (let ((list (cdr list)))
                ,(if end 'list `(match ,(cdr exp))))))

(defun generate-bind-code (exp end)
   (print `(wtf ,exp))
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
          (awhen (gethash (caar exp) destr-match-clauses)
                 (funcall it (cdar exp) (cdr exp) end-of-chain?)) ;; ew 
          (if (stringp (car exp))
              (generate-match-code exp end-of-chain?)
              (generate-bind-code exp end-of-chain?)))))

;; clauses
;;   switch - matches forms, takes the first to work, analogous to parmesan choice
;;   any - causes it to match zero or more list elements
;;   single - causes it to match only one form
;;   optional - if it matches, take that out of the list, keep matching either way,
;;     putting a single binding var at the end will break everything, as usual
;;   test - means the form must match the condition described as well as the structure of the match
;;   key - same as test, except the result of the function is bound instead of the normal result
;;   on-fail - expression to be returned by the whole thing if that subform fails to match

(def-match-clause optional (forms rest end)
   (if end
       `(match ,forms)
       (with-gensyms (foo)
          `(let ((,foo (match ,(append forms rest))))
                 (if ,foo ,foo (match ,rest))))))

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
                     (setf ,w (let ((list ,x)) (match ,(cdr exp))))
                     (when (and (eq ,w t) (funcall ,(second var) ,w)) (go ,succeed))
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

(defun simple-pair (list val)
   (mapcar (lambda (x) (list x val)) list))

;; general process
;;   process the match-form
;;   expand macros, warning or error on actual functions
;;   parse out the relevent symbols
;;   recursive expansion of clauses
;;   generate the full form, inside a let
(defmacro destructuring-match (exp match-form &rest body)
   (let ((match-form (macroexpand `(match ,match-form)))
         (bindings (parse-out-free-symbols match-form)))       
         `(let ,(append `((list ,exp)) (simple-pair bindings nil))
            (if ,match-form
                ,@body 
                 nil))))
