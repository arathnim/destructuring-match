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
;; [x] fix indentation and maybe line length
;; [w] single/multiple modes
;; [x] make the variables update during matching
;; [?] scan for free symbols in normal expressions, like (member foo '(bar baz)) 
;;       would need some way of detecting already-bound local variables? 
;; [x] optimize matching more
;;       store list once, then start cdr'ing list
;;       find a quicker way to build up lists than append
;; [?] another layer of expansion so you can overload macro names
;; [x] remove this end of chain stuff

(defparameter destr-match-clauses (make-hash-table :test #'equalp))
(defparameter matching-style 'symbol) ;; symbol, string, case-sensitive, regex, symbol-regex
(defparameter single-mode nil)

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

(defun generate-match-code (exp)
  `(when (and list ,(match-test (car exp)))
        ,(if (not (cdr exp)) 't
           `(let ((list (cdr list)))
                  (match ,(cdr exp))))))

(defun generate-atom-code (exp)
  `(when (and list (equalp ,(car exp) (car list)))
        ,(if (not (cdr exp)) 't
           `(let ((list (cdr list)))
                  (match ,(cdr exp))))))

(defun bind-single (sym rest type &optional fun)
  `(when ,(if (eq type 'normal) 
             `(car list) 
             `(and (car list) (funcall ,fun (car list))))
      (setf ,sym (car list))
     ,(if rest 
        `(let ((list (cdr list)))
               (match ,rest))
        `(not (car list)))))

(defun bind-multiple (sym rest type &optional fun)
   (if (not rest)
      `(when list (setf ,sym list) t)
       (with-gensyms (x w r p f loop succeed fail end)
         `(let ((,x list) (,p nil) (,w nil) (,r nil))
                (setf ,sym (list (car list)))
                (setf ,p ,sym)
                (tagbody
                  ,loop
                      (setf list (cdr list))
                      (when (not list) (go ,fail))
                      (setf ,w (match ,rest))
                     ,(case type
                         (normal `(when (eq ,w t) (go ,succeed)))
                         (test   `(when (and (eq ,w t) (funcall ,fun ,sym)) (go ,succeed)))
                         (key    `(let ((,f (funcall ,fun ,sym)))
                                        (when (and (eq ,w t) ,f)
                                              (setf ,sym ,f) (go ,succeed)))))
                      (setf ,p (setf (cdr ,p) (cons (car list) nil)))
                      (go ,loop)
                  ,fail
                      (setf ,sym nil)
                      (setf ,r nil)
                      (go ,end)
                  ,succeed
                      (setf ,r ,w)
                  ,end
                      (setf list ,x))
               ,r))))

;; here be dragons
(defun generate-bind-body (sym rest type &optional fun)
   (if single-mode
       (bind-single sym rest type fun)
       (bind-multiple sym rest type fun)))

(defmacro match (exp)
   (if (listp (car exp))
       (aif (gethash (string (caar exp)) destr-match-clauses)
            (funcall it (cdar exp) (cdr exp))
            (error "stuff is seriously messed up here: ~a" exp)) 
       (if (stringp (car exp))
           (generate-match-code exp)
           (if (symbol? (car exp)) 
               (generate-bind-body (car exp) (cdr exp) 'normal)
               (generate-atom-code exp)))))

;; clauses
;;   choice - matches forms, takes the first to work, analogous to parmesan choice
;;   single - causes it to match only one form
;;   multiple - causes it to match any number of forms
;;   optional - if it matches, take that out of the list, keep matching either way
;;   test - the form must match the condition described as well as the structure of the match
;;   key - same as test, except the result of the function is bound instead

(def-match-clause optional (forms rest)
   (if (not rest)
      `(match ,forms)
       (with-gensyms (foo)
          `(let ((,foo (match ,(append forms rest))))
                 (if ,foo ,foo (match ,rest))))))

(def-match-clause choice (forms rest)
   (setf (car forms) (if (not (listp (car forms))) (list (car forms)) (car forms)))
   (if (not (cdr forms))
       (if (not rest) 
         `(match ,(car forms))
         `(match ,(append (car forms) rest)))
       (if (not rest)
           (with-gensyms (foo)
             `(let ((,foo (match ,(first forms))))
                    (if ,foo ,foo (match ((choice ,@(cdr forms)))))))
           (with-gensyms (foo)
             `(let ((,foo (match ,(append (car forms) rest))))
                    (if ,foo ,foo (match ,(append `((choice ,@(cdr forms))) rest))))))))

(def-match-clause single (var rest)
   (bind-single (car var) rest 'normal))

(def-match-clause test (var rest)
   (if (and (listp (first var)) (symbol-eq (car (first var)) 'single))
       (bind-single (second (first var)) rest 'test (second var))
       (if (not rest)
          `(when (and list (funcall ,(second var) list)) (setf ,(first var) list) t)
           (generate-bind-body (car var) rest 'test (second var)))))

(def-match-clause key (var rest)
   (if (and (listp (first var)) (symbol-eq (car (first var)) 'single))
       (bind-single (second (first var)) rest 'test (second var))
       (if (not rest)
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
   (when (and (symbolp (car match-form)) 
              (gethash (string (car match-form)) destr-match-clauses))
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
