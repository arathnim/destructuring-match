(proclaim '(optimize (speed 3) (safety 0) (debug 0) (space 0)))

(ql:quickload '(alexandria iterate anaphora cl-ppcre) :silent t)

(defpackage destr-match
  (:nicknames destructuring-match)
  (:use cl alexandria iterate anaphora)
  (:export destructuring-match defun-match destructuring-match-switch))
(in-package destr-match)

;; TODO
;; [ ] fill out the README with examples and documentation
;; [ ] another layer of expansion so you can overload macro names
;;        primitives vs. clauses?
;; [ ] key macros
;; [ ] short aliases - bind, switch, defun, defmacro
;; [ ] type primitive (type name value)

(defparameter destr-match-clauses (make-hash-table :test #'equalp))
(defparameter string-mode 'string)
(defparameter binding-mode 'mixed)

(defmacro def-match-clause (name ll &rest rest)
   `(setf (gethash ,(string name) destr-match-clauses) (lambda ,ll ,@rest)))

(defun symbol? (exp) (and (symbolp exp) (not (keywordp exp))))

(defun symbol-eq (x y) (string= (string x) (string y)))

(defvar free-symbols nil)

(defun expand-macros (exp)
   (if (and (listp exp) (symbol? (car exp)))
       (or (when (gethash (string (car exp)) destr-match-clauses)
                 (cond ((member (car exp) '(test take) :test #'symbol-eq) 
                          (list (first exp) (expand-macros (second exp)) (third exp)))
                       ((symbol-eq 'quote (car exp)) (return-from expand-macros exp))
                       (t (cons (car exp) (mapcar #'expand-macros (cdr exp))))))
           (multiple-value-bind (a b) (macroexpand-1 exp)
              (when b (expand-macros a)))
           (mapcar #'expand-macros exp))
       (if (symbol? exp)
           (progn (appendf free-symbols (list exp)) exp)
           (if (listp exp)
               (mapcar #'expand-macros exp)
               exp))))

(defun expand-main (exp)
   (let* ((free-symbols nil)
          (exp (expand-macros exp)))
          (values exp (delete-duplicates free-symbols))))

(defun match-test (str)
   (case string-mode
      (string         `(equalp  ,str (car list)))
      (symbol         `(eq ',(intern (string-upcase str)) (car list)))
      (case-sensitive `(string= ,str (car list)))
      (regex          `(cl-ppcre:scan ,str (car list)))
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
        `(not (cdr list)))))

;; here be dragons
(defun bind-multiple (sym rest type &optional fun strip)
   (if (not rest)
      `(when list (setf ,sym (if (not (cdr list)) (car list) list)) t)
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
   (case binding-mode
      (single (bind-single sym rest type fun))
      (multiple (bind-multiple sym rest type fun))
      (mixed (bind-multiple sym rest type fun t))
      (otherwise (error "invalid binding-mode: ~a" binding-mode))))

(defmacro match (exp)
   (print `(match ,exp))
   (if (listp (car exp))
       (aif (gethash (string (caar exp)) destr-match-clauses)
            (funcall it (cdar exp) (cdr exp))
            (when exp `(match ((sublist ,@(car exp)) ,@(cdr exp))))) 
       (if (stringp (car exp))
           (generate-match-code exp)
           (if (symbol? (car exp)) 
               (generate-bind-body (car exp) (cdr exp) 'normal)
               (generate-atom-code exp)))))

;; clauses

(def-match-clause quote (forms rest)
   `(when (and list (eq ',(car forms) (car list)))
         ,(if (not rest) 't
             `(let ((list (cdr list)))
                    (match ,rest)))))

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

(def-match-clause multiple (var rest)
   (bind-multiple (car var) rest 'normal))

(def-match-clause test (var rest)
   (if (and (listp (first var)) (symbol-eq (car (first var)) 'single))
       (bind-single (second (first var)) rest 'test (second var))
       (if (not rest)
          `(when (and list (funcall ,(second var) list)) (setf ,(first var) list) t)
           (generate-bind-body (car var) rest 'test (second var)))))

(def-match-clause take (var rest)
   (if (and (listp (first var)) (symbol-eq (car (first var)) 'single))
       (bind-single (second (first var)) rest 'test (second var))
       (if (not rest)
          `(awhen (and list (funcall ,(second var) list)) (setf ,(first var) it) t)
           (generate-bind-body (car var) rest 'take (second var)))))

(def-match-clause sublist (forms rest)
   (if (not rest) 
      `(when (and (car list) (listp (car list)))
             (when (let ((list (car list))) (match ,forms)) t))
      `(when (and (car list) (listp (car list)))
             (when (let ((list (car list))) (match ,forms))
                   (let ((list (cdr list))) (match ,rest))))))

(defun simple-pair (list val)
   (mapcar (lambda (x) (list x val)) list))

(defmacro destructuring-match (&rest body)
   (let ((fail nil))
    (setf string-mode 'string)
    (setf binding-mode 'mixed)
    (iter (for x from 0 to (length body) by 2)
          (cond ((eq :string-mode (elt body x))
                 (setf string-mode (elt body (+ x 1))))
                ((eq :on-failure (elt body x))
                 (setf fail (elt body (+ x 1))))
                ((eq :binding-mode (elt body x))
                 (setf binding-mode (elt body (+ x 1))))
                (t (setf body (subseq body x)) (finish))))
    (destructuring-bind (exp match-form &rest rest) body
      (multiple-value-bind (match-form bindings) (expand-main match-form)
        `(let ,(append `((list ,exp)) (simple-pair bindings nil))
           (if (match ,match-form)
               (progn ,@rest) 
               ,fail))))))

(defmacro destructuring-match-switch (exp &rest forms)
   (with-gensyms (f)
      `(let ((,f ,exp)) 
             (or ,@(mapcar (lambda (x) `(destructuring-match ,f ,(car x) ,@(cdr x))) forms)))))

;; NOTE make a special version of the main macro that errors on non-match!
(defmacro defun-match (name ll &rest body)
   (with-gensyms (args)
      `(progn (eval-when (:compile-toplevel) (sb-c:%compiler-defun ',name nil t)) 
              (sb-impl::%defun ',name 
                (sb-int:named-lambda ,name (&rest ,args)
                  (block ,name 
                     (destructuring-match :on-failure (error "couldn't match args!") 
                        ,args ,ll ,@body)))
                (sb-c:source-location)))))
