(proclaim '(optimize (speed 3) (safety 0) (debug 0) (space 0)))
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))

(ql:quickload '(alexandria iterate anaphora cl-ppcre) :silent t)

(defpackage destr-match
  (:nicknames destructuring-match)
  (:use cl alexandria iterate anaphora)
  (:export destructuring-match defun-match destructuring-match-switch))
(in-package destr-match)

;; TODO
;; [w] fill out the README with examples and documentation
;; [w] sublists
;; [ ] modify match so clause names without arguments are bindings
;; [?] full literal value matching, using quotes for symbols
;; [w] fix indentation and maybe line length
;; [w] single/multiple modes
;; [w] make the variables update during matching
;; [?] scan for free symbols in normal expressions, like (member foo '(bar baz)) 
;;       would need some way of detecting already-bound local variables? 
;; [?] optimize matching more
;; [ ] another layer of expansion so you can overload macro names

(defparameter destr-match-clauses (make-hash-table :test #'equalp))
(defparameter matching-style 'symbol) ;; symbol, string, case-sensitive, regex, symbol-regex
(defparameter single-mode t)

(defmacro def-match-clause (name ll &rest rest)
   `(setf (gethash ,(string name) destr-match-clauses) (lambda ,ll ,@rest)))

(defun symbol? (exp) (and (symbolp exp) (not (keywordp exp))))

(defun symbol-eq (x y) (string= (string x) (string y)))

(defvar free-symbols nil)
(defun expand-main (exp)
   (let* ((free-symbols nil)
          (exp (expand-macros exp)))
          (values exp (delete-duplicates free-symbols))))

(defun expand-macros (exp)
   (if (and (listp exp) (symbol? (car exp)))
       (or (when (gethash (string (car exp)) destr-match-clauses) 
                 (if (member (car exp) '(test key) :test #'symbol-eq) 
                     (list (first exp) (expand-macros (second exp)) (third exp)) 
                     (cons (car exp) (mapcar #'expand-macros (cdr exp)))))
           (multiple-value-bind (a b) (macroexpand-1 exp)
              (when b (expand-macros a)))
           (mapcar #'expand-macros exp))
       (if (symbol? exp) 
           (progn (appendf free-symbols (list exp)) exp)
           (if (listp exp) 
               (mapcar #'expand-macros exp)
               exp))))

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

(defun generate-atom-code (exp end)
  `(when (and list (equalp ,(car exp) (car list)))
        ,(if end 't
           `(let ((list (cdr list)))
                  (match ,(cdr exp))))))

;; here be dragons
(defun generate-bind-body (sym rest type &optional fun)
   (if single-mode
       nil
       (with-gensyms (x w r f loop succeed fail end)
         `(let ((,x (cdr list)) (,w nil) (,r nil))
                (setf ,sym (list (car list)))
                (tagbody
                  ,loop
                      (when (not ,x) (go ,fail))
                      (setf ,w (let ((list ,x)) (match ,rest)))
                     ,(case type
                         (normal `(when (eq ,w t) (go ,succeed)))
                         (test   `(when (and (eq ,w t) (funcall ,fun ,sym)) (go ,succeed)))
                         (key    `(let ((,f (funcall ,fun ,sym)))
                                        (when (and (eq ,w t) ,f)
                                              (setf ,sym ,f) (go ,succeed)))))
                      (appendf ,sym (list (car ,x)))
                      (setf ,x (cdr ,x))
                      (go ,loop)
                  ,fail
                      (setf ,sym nil)
                      (setf ,r nil)
                      (go ,end)
                  ,succeed
                      (setf ,r ,w)
                  ,end)
               ,r))))

(defun generate-bind-code (exp end)
   (if end
      `(when list (setf ,(car exp) list) t)
       (generate-bind-body (car exp) (cdr exp) 'normal)))

(defmacro match (exp)
   (let ((end-of-chain? (not (cdr exp))))
      (if (listp (car exp))
          (aif (gethash (string (caar exp)) destr-match-clauses)
               (funcall it (cdar exp) (cdr exp) end-of-chain?)
               (error "stuff is seriously messed up here: ~a" exp)) 
          (if (stringp (car exp))
              (generate-match-code exp end-of-chain?)
              (if (symbol? (car exp)) 
                  (generate-bind-code exp end-of-chain?)
                  (generate-atom-code exp end-of-chain?))))))

;; clauses
;;   choice - matches forms, takes the first to work, analogous to parmesan choice
;;   single - causes it to match only one form
;;   multiple - causes it to match any number of forms
;;   optional - if it matches, take that out of the list, keep matching either way
;;   test - the form must match the condition described as well as the structure of the match
;;   key - same as test, except the result of the function is bound instead

(def-match-clause optional (forms rest end)
   (if end
      `(match ,forms)
       (with-gensyms (foo)
          `(let ((,foo (match ,(append forms rest))))
                 (if ,foo ,foo (match ,rest))))))

(def-match-clause choice (forms rest end)
   (setf (car forms) (if (not (listp (car forms))) (list (car forms)) (car forms)))
   (if (not (cdr forms))
       (if end 
         `(match ,(car forms))
         `(match ,(append (car forms) rest)))
       (if end
           (with-gensyms (foo)
             `(let ((,foo (match ,(first forms))))
                    (if ,foo ,foo (match ((choice ,@(cdr forms)))))))
           (with-gensyms (foo)
             `(let ((,foo (match ,(append (car forms) rest))))
                    (if ,foo ,foo (match ,(append `((choice ,@(cdr forms))) rest))))))))

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

(defun test-single (var rest end)
   (if end
      `(when (and (car list) (funcall ,(second var) (car list)))
             (setf ,(second (first var)) (car list))
             (not (cdr list)))
      `(when (and (car list) (funcall ,(second var) (car list)))
             (setf ,(second (first var)) (car list))
             (let ((list (cdr list)))
                   (match ,rest)))))

(def-match-clause test (var rest end)
   (if (and (listp (first var)) (symbol-eq (car (first var)) 'single))
       (test-single var rest end)
   (if end
      `(when (and list (funcall ,(second var) list)) (setf ,(first var) list) t)
       (generate-bind-body (car var) rest 'test (second var)))))

(defun key-single (var rest end)
   (if end
       `(awhen (and (car list) (funcall ,(second var) (car list)))
               (setf ,(second (first var)) it)
               (not (cdr list)))
       `(awhen (and (car list) (funcall ,(second var) (car list)))
               (setf ,(second (first var)) it)
               (let ((list (cdr list)))
                     (match ,rest)))))

(def-match-clause key (var rest end)
   (if (and (listp (first var)) (symbol-eq (car (first var)) 'single))
      (key-single var rest end)
      (if end
         `(awhen (and list (funcall ,(second var) list)) (setf ,(first var) it) t)
          (generate-bind-body (car var) rest 'key (second var)))))

(defun simple-pair (list val)
   (mapcar (lambda (x) (list x val)) list))

(defmacro destructuring-match (exp match-form &rest body)
   (when (eq exp :mode)
         (setf matching-style match-form)
         (setf exp (first body))
         (setf match-form (second body))
         (setf body (cddr body)))
   (when (gethash (string (car match-form)) destr-match-clauses)
         (setf match-form (list match-form)))
   (multiple-value-bind (match-form bindings) (expand-main match-form)
      `(let ,(append `((list ,exp)) (simple-pair bindings nil))
             (if (match ,match-form)
                 ,@body 
                  nil))))

(defmacro destructuring-match-switch (exp &rest forms)
   (with-gensyms (f)
      `(let ((,f ,exp)) 
             (or 
               ,@(mapcar 
                  (lambda (x) `(destructuring-match ,f ,(car x) ,@(cdr x))) 
                  forms)))))

;; bloody magic. This could probably actually be used to replace defun
;; also, sb-impl::%defun was defined on a lisp machine. check the description.
;; NOTE make a special version of the main macro that errors on non-match!
;;      maybe a way to change the mode/style as well
(defmacro defun-match (name ll &rest body)
   (with-gensyms (args)
      `(progn (eval-when (:compile-toplevel) (sb-c:%compiler-defun ',name nil t)) 
              (sb-impl::%defun ',name 
                (sb-int:named-lambda ,name (&rest ,args)
                  (block ,name 
                     (destructuring-match ,args ,ll ,@body)))
                (sb-c:source-location)))))
