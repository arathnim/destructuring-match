(proclaim '(optimize (speed 3) (safety 0) (debug 0) (space 0)))

(ql:quickload '(alexandria iterate anaphora cl-ppcre) :silent t)

(defpackage destr-match-core
  (:use cl iterate anaphora))

(defpackage destr-match
  (:nicknames destructuring-match)
  (:use cl)
  (:export bind match))

(defpackage destr-match-extras
  (:shadow defun defmacro)
  (:use cl)
  (:export defun defmacro))

(in-package destr-match-core)

;; TODO
;; struct matching
;; multiple-bind for test and key
;; move wrapping/desugaring code to match
;; better error handling during macroexpand-time

(defparameter clauses (make-hash-table :test #'equalp))
(defparameter primitives (make-hash-table :test #'equalp))
(defvar string-mode nil)
(defvar binding-mode nil)

(defmacro def-match-clause (name ll &rest rest)
   `(setf (gethash ,(string name) clauses) (lambda ,ll ,@rest)))

(defmacro def-match-primitive (name ll &rest rest)
   `(setf (gethash ,(string name) primitives) (lambda ,ll ,@rest)))

(defun symbol? (exp) (and (symbolp exp) (not (keywordp exp))))

(defun symbol-eq (x y)
   "Compares the names of symbols, to prevent package issues"
   (and (symbolp x) (symbolp y) (string= (string x) (string y))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(iter (for n in names) (collect `(,n (gensym))))
        ,@body))

(defvar free-symbols nil)

(defun expand-macros (exp)
   "Expand clauses and macros, append free symbols into free-symbols"
   (if (and (listp exp) (symbol? (car exp)))
       (or (when (gethash (string (car exp)) primitives)
                 (cond ((member (car exp) '(test take) :test #'symbol-eq) 
                          (list (first exp) (expand-macros (second exp)) (third exp)))
                       ((symbol-eq 'quote (car exp)) (return-from expand-macros exp))
                       (t (cons (car exp) (mapcar #'expand-macros (cdr exp))))))
           (awhen (gethash (string (car exp)) clauses)
                  (expand-macros (apply it (cdr exp))))
           (multiple-value-bind (a b) (macroexpand-1 exp)
              (when b (expand-macros a)))
           (mapcar #'expand-macros exp))
       (if (symbol? exp)
           (progn (setf free-symbols (append free-symbols (list exp))) exp)
           (if (listp exp)
               (mapcar #'expand-macros exp)
               exp))))

(defun expand-main (exp)
   "Expands matching code, collects free symbols for binding"
   (let* ((free-symbols nil)
          (exp (expand-macros exp)))
          (values exp (delete-duplicates free-symbols))))

(defun string-test (str)
   "Generates the correct code for string comparison from string-mode"
   (case string-mode
      (string         `(equalp  ,str (car list)))
      (symbol         `(eq ',(intern (string-upcase str)) (car list)))
      (case-sensitive `(string= ,str (car list)))
      (regex          `(cl-ppcre:scan ,str (car list)))
      (otherwise       (error "something went very wrong"))))

(defun generate-atom-code (exp)
   "Generates the code to match all atoms"
  `(when ,(if (stringp (car exp)) 
              (string-test (car exp)) 
             `(equalp ,(car exp) (car list)))
         ,(if (not (cdr exp)) 't
            `(let ((list (cdr list)))
                  ,(match (cdr exp))))))

(defun bind-single (sym rest type &optional fun)
   "Binds a single element of the list to sym"
  `(when ,(if (eq type 'normal) 
             `(car list) 
             `(and (car list) (funcall ,fun (car list))))
      (setf ,sym (car list))
     ,(if rest 
        `(let ((list (cdr list)))
              ,(match rest))
        `(not (cdr list)))))

;; here be dragons. bad ones.
(defun bind-multiple (sym rest type &optional fun strip)
   "Binds multiple elements of the list to sym, as determined by the structure"
   (if (not rest)
      `(when list (setf ,sym (if (and ,strip (not (cdr list))) (car list) list)) t)
       (with-gensyms (x w r p f loop succeed fail end)
         `(let ((,x list) (,p nil) (,w nil) (,r nil))
                (setf ,sym (list (car list)))
                (setf ,p ,sym)
                (tagbody
                  ,loop
                      (setf list (cdr list))
                      (when (not list) (go ,fail))
                      (setf ,w ,(match rest))
                     ,(case type
                         (normal `(when (eq ,w t) (go ,succeed)))
                         (test   `(when (and (eq ,w t) (funcall ,fun ,sym)) (go ,succeed)))
                         (take   `(let ((,f (funcall ,fun ,sym)))
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
                     ,(when strip `(and (listp ,sym) (not (cdr ,sym)) (setf ,sym (car ,sym))))
                  ,end
                      (setf list ,x))
               ,r))))

(defun generate-bind-body (sym rest type &optional fun)
   "Dispatches to bind-single or bind-multiple based on binding-mode"
   (alexandria:switch (binding-mode :test #'symbol-eq)
      ('single (bind-single sym rest type fun))
      ('multiple (bind-multiple sym rest type fun))
      ('mixed (bind-multiple sym rest type fun t))
      (otherwise (error "invalid binding-mode: ~a" binding-mode))))

(defun match (exp)
   "Base of the macro, calls primitives and binds variables"
   (if (listp (car exp))
       (aif (gethash (string (caar exp)) primitives)
            (funcall it (cdar exp) (cdr exp))
            (when exp (match `((sublist ,@(car exp)) ,@(cdr exp))))) 
       (if (symbol? (car exp))
           (generate-bind-body (car exp) (cdr exp) 'normal)
           (generate-atom-code exp))))

;; primitives

(def-match-primitive quote (forms rest)
   `(when (symbol-eq ',(car forms) (car list))
         ,(if (not rest) `(not (cdr list))
             `(let ((list (cdr list)))
                   ,(match rest)))))

(def-match-primitive optional (forms rest)
   (with-gensyms (foo)
     `(let ((,foo ,(match (if (not rest) forms (append forms rest)))))
        (if ,foo ,foo ,(if (not rest) t (match rest))))))

(def-match-primitive choice (forms rest)
   (when (or (not (listp (car forms))) (eq (caar forms) 'quote))
         (setf (car forms) (list (car forms))))
   (if (not (cdr forms))
       (match (if (not rest) (car forms) (append (car forms) rest)))
       (with-gensyms (foo)
         `(let ((,foo ,(match (if (not rest) (first forms) (append (car forms) rest)))))
                (if ,foo ,foo
                    ,(match (if (not rest) 
                               `((choice ,@(cdr forms))) 
                                (append `((choice ,@(cdr forms))) rest))))))))

(def-match-primitive single (var rest)
   (bind-single (car var) rest 'normal))

(def-match-primitive multiple (var rest)
   (bind-multiple (car var) rest 'normal))

(def-match-primitive test (var rest)
   (print `(test ,var ,rest))
   (if (and (listp (first var)) (symbol-eq (car (first var)) 'single))
       (bind-single (second (first var)) rest 'test (second var))
       (if (not rest)
          `(when (and list (funcall ,(second var) list)) (setf ,(first var) list) t)
           (generate-bind-body (car var) rest 'test (second var)))))

(def-match-primitive take (var rest)
   (if (and (listp (first var)) (symbol-eq (car (first var)) 'single))
       (bind-single (second (first var)) rest 'test (second var))
       (if (not rest)
          `(awhen (and list (funcall ,(second var) list)) (setf ,(first var) it) t)
           (generate-bind-body (car var) rest 'take (second var)))))

(def-match-primitive sublist (forms rest)
   `(when (and (car list) (listp (car list)))
      (when (let ((list (car list))) ,(match forms))
         ,(if (not rest) t `(let ((list (cdr list))) ,(match rest))))))

(defun simple-pair (list val)
   (mapcar (lambda (x) (list x val)) list))

(defmacro bind (&rest body)
   "(key string-mode binding-mode on-failure) match-form expression body"
   (let ((fail nil) (string-mode 'string) (binding-mode 'mixed))
    (iter (for x from 0 to (length body) by 2)
          (cond ((eq :string-mode (elt body x))
                 (setf string-mode (elt body (+ x 1))))
                ((eq :on-failure (elt body x))
                 (setf fail (elt body (+ x 1))))
                ((eq :binding-mode (elt body x))
                 (setf binding-mode (elt body (+ x 1))))
                (t (setf body (subseq body x)) (finish))))
    (destructuring-bind (match-form exp &rest rest) body
      (multiple-value-bind (match-form bindings) (expand-main match-form)
         (when (and (listp match-form) (symbol? (car match-form)) 
                    (gethash (string (car match-form)) primitives))
               (setf match-form (list match-form)))
        `(let ,(append `((list ,exp)) (simple-pair bindings nil))
           (if (and (listp list) ,(match match-form))
                (progn ,@rest)
               ,fail))))))

;; some helpful clauses

(defun as-keyword (symbol)
   "Returns a keyword with the same name as symbol"
   (intern (symbol-name symbol) :keyword))

(defun generate-key (var)
   "Generate the code to match a single key"
   (if (symbolp var)
      `(,(as-keyword var) (single ,var)) ;; (:sym sym)
       (if (not (listp var))
           (error "weird stuff in key clause: ~a" var) 
           (destructuring-bind (sym &key names and-key) var
              (if (or names and-key)
                 `((test ,(if and-key `(single ,and-key) `(single ,(gensym)))
                         (lambda (x) (member x ',(mapcar #'as-keyword names)))) (single ,sym))
                  (generate-key sym))))))

(defun generate-key-structure (forms ord)
   (if (not (cdr forms))
      `(optional ,@(generate-key (car forms))) 
       (if ord
          `(optional
            ,@(append (generate-key (car forms)) (generate-key-structure (cdr forms) ord)))
          `(optional (choice
            ,@(iter (for x in forms)
                    (collect
                     (append (generate-key x)
                             (list (generate-key-structure (remove x forms) ord))))))))))

(def-match-clause key (&rest args)
   (let ((ord nil))
         (when (eq (car args) :ordered)
               (setf ord t)
               (setf args (cdr args)))
         (generate-key-structure args ord)))

;; package dark magic and level hacking starts here

(in-package destr-match)

(cl:defmacro bind (&rest forms)
   `(destr-match-core::bind ,@forms))

;; needs some way to pass key arguments
(cl:defmacro match (exp &rest forms)
   (alexandria:with-gensyms (f)
      `(let ((,f ,exp))
             (or ,@(mapcar (lambda (x) `(destr-match-core::bind ,(car x) ,f ,@(cdr x))) 
                           forms)))))

(in-package destr-match-extras)

(cl:defmacro defun (name ll &rest body)
   (alexandria::with-gensyms (args)
      `(cl:defun ,name (&rest ,args)
         ,(when (stringp (car body)) 
                (let ((str (car body))) (setf body (cdr body)) str))
          (destr-match-core::bind :on-failure (error "couldn't match args")
            ,args ,ll ,@body))))

(cl:defmacro defmacro (name ll &rest body)
   (alexandria:with-gensyms (args)
      `(cl:defmacro ,name (&rest ,args)
         ,(when (stringp (car body)) 
                (let ((str (car body))) (setf body (cdr body)) str))
          (destr-match-core::bind :on-failure (error "couldn't match args")
            ,args ,ll ,@body))))
