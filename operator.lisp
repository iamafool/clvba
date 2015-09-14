(in-package #:clvba)
(in-readtable :clvba)


;;; CLVBA operators and macros that aren't present in the Common Lisp
;;; standard but exported by CLVBA, and their Common Lisp
;;; equivalent definitions

(defmacro define-trivial-special-ops (&rest mappings)
  `(progn ,@(loop for (form-name vba-primitive) on mappings by #'cddr collect
                 `(define-expression-operator ,form-name (&rest args)
                    (cons ',vba-primitive (mapcar #'compile-expression args))))))

(define-trivial-special-ops
  array      clvba-vba:array
  &          clvba-vba:&
  )


(define-statement-operator switch (test-expr &rest clauses)
  `(clvba-vba:select-case ,(compile-expression test-expr)
     ,@(loop for (val . body) in clauses collect
            (cons (if (eq val 'default)
                      'clvba-vba:default
                      (let ((in-case? t))
                        (compile-expression val)))
                  (mapcan (lambda (x)
                            (let* ((in-case? t)
                                   (exp      (compile-statement x)))
                              (if (and (listp exp) (eq 'clvba-vba:block (car exp)))
                                  (cdr exp)
                                  (list exp))))
                          body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; objects

(define-expression-operator %vba-getprop (obj slot)
  (let ((expanded-slot (clvba-macroexpand slot))
        (obj (compile-expression obj)))
    (if (and (listp expanded-slot)
             (eq 'quote (car expanded-slot)))
        (aif (or (reserved-symbol-p (second expanded-slot))
                 (and (keywordp (second expanded-slot)) (second expanded-slot)))
             `(clvba-vba:aref ,obj ,it)
             `(clvba-vba:getprop ,obj ,(second expanded-slot)))
        `(clvba-vba:aref ,obj ,(compile-expression slot)))))

(defclvbamacro getprop (obj &rest slots)
  (if (null (rest slots))
      `(%vba-getprop ,obj ,(first slots))
      `(getprop (getprop ,obj ,(first slots)) ,@(rest slots))))

(defclvbamacro @ (obj &rest props)
  "Handy getprop/aref composition macro."
  (if props
      `(@ (getprop ,obj ,(if (symbolp (car props))
                             `',(car props)
                             (car props)))
          ,@(cdr props))
      obj))

(defclvbamacro chain (&rest method-calls)
  (labels ((do-chain (method-calls)
             (if (cdr method-calls)
                 (if (listp (car method-calls))
                     `((@ ,(do-chain (cdr method-calls)) ,(caar method-calls)) ,@(cdar method-calls))
                     `(@ ,(do-chain (cdr method-calls)) ,(car method-calls)))
                 (car method-calls))))
    (do-chain (reverse method-calls))))


;;; arg
(define-expression-operator arg (name &key optional by paramarray type defaultvalue)
  `(clvba-vba:arg ,(clvba-macroexpand name) :optional ,optional :by ,by :paramarray ,paramarray :type ,type :defaultvalue ,defaultvalue)) 

;;; dim
(define-statement-operator dim (name &key as (docstr ""))
  `(clvba-vba:dim ,(clvba-macroexpand name) :type ,as :documentation ,docstr :scope "dim"))

(define-expression-operator dim (name &key as (docstr ""))
  `(clvba-vba:dim ,(clvba-macroexpand name) :type ,as :documentation ,docstr :scope nil))

;;; public
(define-statement-operator public (name &key as (docstr ""))
  `(clvba-vba:dim ,(clvba-macroexpand name) :type ,as :documentation ,docstr :scope "public"))

;;; private
(define-statement-operator private (name &key as (docstr ""))
  `(clvba-vba:dim ,(clvba-macroexpand name) :type ,as :documentation ,docstr :scope "private"))

;;; redim
(define-statement-operator redim (name &key as preserve)
  `(clvba-vba:redim ,(clvba-macroexpand name) :type ,as :preserve t))

;;; iteration

(define-statement-operator for ((var start end &optional step) &body body)
  `(clvba-vba:for ,(compile-expression var)
              ,(compile-expression start)
              ,(compile-expression end)
              ,(compile-expression step)
              ,(compile-loop-body (list var) body)))

(define-statement-operator for-in ((var object) &rest body)
  `(clvba-vba:for-in ,(compile-expression var)
                 ,(compile-expression object)
                 ,(compile-loop-body (list var) body)))

(define-statement-operator while (test &rest body)
  `(clvba-vba:while ,(compile-expression test)
     ,(compile-loop-body () body)))

(define-statement-operator with (expression &rest body)
  `(clvba-vba:with ,(compile-expression expression)
     ,(compile-loop-body () body)))

(defmacro while (test &body body)
  `(loop while ,test do (progn ,@body)))

;;; misc

(define-expression-operator lisp (lisp-form)
  ;; (clvba (foo (lisp bar))) is like (clvba* `(foo ,bar))
  ;; When called from inside of clvba*, lisp-form has access to the
  ;; dynamic environment only, analogous to eval.
  `(clvba-vba:escape
    (with-output-to-string (*clvbaw-stream*)
      (let ((compile-expression? ,compile-expression?))
        (cl-vba-print (clvba-compile ,lisp-form) t)))))

(defun lisp (x) x)




(defclvbamacro stringify (&rest things)
  (if (and (= (length things) 1) (stringp (car things)))
      (car things)
      `((@ (list ,@things) :join) "")))

(defun stringify (&rest things)
  "Like concatenate but prints all of its arguments."
  (format nil "~{~A~}" things))

(define-clvba-symbol-macro false clvba-vba:false)
(defvar false nil)


;; raw
(define-statement-operator raw (x)
  `(clvba-vba:raw ,x))

(define-expression-operator raw (x)
  `(clvba-vba:raw ,x))

(define-statement-operator subcall (&rest args)
  `(clvba-vba:block
       (clvba-vba:subcall ,@(mapcar #'compile-expression args))))

(define-statement-operator set (var-name var-value)
  `(clvba-vba:set ,(compile-expression var-name) ,(compile-expression var-value)))
    
(define-expression-operator |:=| (var value)
  `(clvba-vba:|:=| ,var ,(compile-expression value)))

(define-expression-operator to (x y)
  `(clvba-vba:to ,x ,y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; arithmetic and logic

(define-trivial-special-ops
  +          clvba-vba:+
  -          clvba-vba:-
  *          clvba-vba:*
  mod        clvba-vba:mod
  and        clvba-vba:and
  or         clvba-vba:or
  aref       clvba-vba:aref
  funcall    clvba-vba:funcall
  subcall    clvba-vba:subcall
  is         clvba-vba:is
  )

(define-expression-operator / (&rest args)
  `(clvba-vba:/ ,@(unless (cdr args) (list 1)) ,@(mapcar #'compile-expression args)))

(define-expression-operator - (&rest args)
  (let ((args (mapcar #'compile-expression args)))
    (cons (if (cdr args) 'clvba-vba:- 'clvba-vba:negate) args)))

(defun fix-nary-comparison (operator objects)
  (let* ((tmp-var-forms (butlast (cdr objects)))
         (tmp-vars (loop repeat (length tmp-var-forms)
                         collect (clvba-gensym "_CMP")))
         (all-comparisons (append (list (car objects))
                                  tmp-vars
                                  (last objects))))
    `(let ,(mapcar #'list tmp-vars tmp-var-forms)
       (and ,@(loop for x1 in all-comparisons
                    for x2 in (cdr all-comparisons)
                    collect (list operator x1 x2))))))

(macrolet ((define-nary-comparison-forms (&rest mappings)
             `(progn
                ,@(loop for (form vba-primitive) on mappings by #'cddr collect
                       `(define-expression-operator ,form (&rest objects)
                          (if (cddr objects)
                              (clvba-compile
                               (fix-nary-comparison ',form objects))
                              (cons ',vba-primitive
                                    (mapcar #'compile-expression objects))))))))
  (define-nary-comparison-forms
    <     clvba-vba:<
    >     clvba-vba:>
    <=    clvba-vba:<=
    >=    clvba-vba:>=
    eql   clvba-vba:=
    equal clvba-vba:=))

(define-expression-operator <> (a b)
  `(clvba-vba:<> ,(compile-expression a) ,(compile-expression b)))

(defun references? (exp place)
  (cond ((not exp) nil)
        ((atom exp) (equal exp place))
        (t (or (equal exp place)
               (references? (car exp) place)
               (references? (cdr exp) place)))))

(let ((inverses (mapcan (lambda (x)
                          (list x (reverse x)))
                        '((clvba-vba:= clvba-vba:<>)
                          (clvba-vba:< clvba-vba:>=)
                          (clvba-vba:> clvba-vba:<=)))))
  (define-expression-operator not (x)
    (let ((form (compile-expression x)))
      (acond ((and (listp form) (eq (car form) 'clvba-vba:not))
              (second form))
             ((and (listp form) (cadr (assoc (car form) inverses)))
              `(,it ,@(cdr form)))
             (t `(clvba-vba:not ,form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; blocks and control flow

(defun flatten-blocks (body)
  (when body
    (if (and (listp (car body)) (eq 'clvba-vba:block (caar body)))
        (append (cdr (car body)) (flatten-blocks (cdr body)))
        (cons (car body) (flatten-blocks (cdr body))))))

(defun compile-progn (body)
  (let ((block (flatten-blocks (mapcar #'clvba-compile body))))
    (append (remove-if #'constantp (butlast block))
            (unless (and (or (eq *compilation-level* :toplevel)
                             (not compile-expression?))
                         (not (car (last block))))
              (last block)))))

(define-expression-operator progn (&rest body)
  (if (cdr body)
      `(clvba-vba:|,| ,@(compile-progn body))
      (compile-expression (car body))))

(define-statement-operator progn (&rest body)
  `(clvba-vba:block ,@(compile-progn body)))

(defun fill-mv-reg (values)
  `(setf __CLVBA_MV_REG (create :tag    (@ arguments callee)
                             :values ,values)))

(defvar suppress-values?)


(define-statement-operator block (name &rest body)
  (if in-function-scope?
      (let* ((name                  (or name 'nilBlock))
             (in-loop-scope?        (if name in-loop-scope? nil))
             (*current-block-tag*   name)
             (compiled-body         (compile-statement `(progn ,@body))))
            compiled-body)
      (clvba-compile (with-lambda-scope `(block ,name ,@body)))))


(defun try-expressionizing-if? (exp &optional (score 0)) ;; poor man's codewalker
  "Heuristic that tries not to expressionize deeply nested if expressions."
  (cond ((< 1 score) nil)
        ((and (listp exp) (eq (car exp) 'quote))
         t)
        ((listp exp)
         (loop for x in (cdr exp) always
              (try-expressionizing-if?
               (or (ignore-errors (clvba-macroexpand x)) x) ;; fail
               (+ score (case (car exp)
                          ((if cond let) 1)
                          ((progn) (1- (length (cdr exp))))
                          (otherwise 0))))))
        (t t)))


(define-expression-operator values (&optional main &rest additional)
  (when main
    (clvba-compile (if additional
                    `(prog1 ,main ,@additional)
                    main))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conditionals

(define-statement-operator if (test then &optional else)
  `(clvba-vba:if ,(compile-expression test)
             ,(compile-statement then)
             ,@(when else
                     `(:else ,(compile-statement else)))))

(define-statement-operator cond (&rest clauses)
  `(clvba-vba:if ,(compile-expression (caar clauses))
             ,(compile-statement `(progn ,@(cdar clauses)))
             ,@(loop for (test . body) in (cdr clauses) appending
                    (if (eq t test)
                        `(:else ,(compile-statement `(progn ,@body)))
                        `(:else-if ,(compile-expression test)
                                   ,(compile-statement `(progn ,@body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros

(defmacro with-local-macro-environment ((var env) &body body)
  `(let* ((,var (make-macro-dictionary))
          (,env (cons ,var ,env)))
     ,@body))

(define-expression-operator macrolet (macros &body body)
  (with-local-macro-environment (local-macro-dict *macro-env*)
    (dolist (macro macros)
      (destructuring-bind (name arglist &body body)
          macro
        (setf (gethash name local-macro-dict)
              (eval (make-clvba-macro-function arglist body)))))
    (clvba-compile `(progn ,@body))))

(define-expression-operator symbol-macrolet (symbol-macros &body body)
  (with-local-macro-environment (local-macro-dict *symbol-macro-env*)
    (let (local-var-bindings)
      (dolist (macro symbol-macros)
        (destructuring-bind (name expansion) macro
          (setf (gethash name local-macro-dict) (lambda (x) (declare (ignore x)) expansion))
          (push name local-var-bindings)))
      (let ((*enclosing-lexicals* (append local-var-bindings *enclosing-lexicals*)))
        (clvba-compile `(progn ,@body))))))

(define-expression-operator defmacro (name args &body body)
  (eval `(defclvbamacro ,name ,args ,@body))
  nil)

(define-expression-operator define-symbol-macro (name expansion)
  (eval `(define-clvba-symbol-macro ,name ,expansion))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assignment

(defun assignment-op (op)
  (getf '() op))

(define-expression-operator clvba-assign (lhs rhs)
  (let ((rhs (clvba-macroexpand rhs)))
    (if (and (listp rhs) (eq (car rhs) 'progn))
        (clvba-compile `(progn ,@(butlast (cdr rhs))
                            (clvba-assign ,lhs ,(car (last (cdr rhs))))))
        (let ((lhs (compile-expression lhs))
              (rhs (compile-expression rhs)))
          (aif (and (listp rhs)
                    (= 3 (length rhs))
                    (equal lhs (second rhs))
                    (assignment-op (first rhs)))
               (list it lhs (if (fourth rhs)
                                (cons (first rhs) (cddr rhs))
                                (third rhs)))
               (list 'clvba-vba:= lhs rhs))))))

(define-statement-operator defvar (name &optional documentation)
  (pushnew name *special-variables*)
  (check-type documentation string "a string")
  (clvba-compile `(dim ,name :as nil :docstr ,documentation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; binding

(defmacro with-declaration-effects ((var block) &body body)
  (let ((declarations (gensym)))
   `(let* ((,var ,block)
           (,declarations (and (listp (car ,var))
                               (eq (caar ,var) 'declare)
                               (cdar ,var)))
           (,var (if ,declarations
                     (cdr ,var)
                     ,var))
           (*special-variables* (append (cdr (find 'special ,declarations :key #'car)) *special-variables*)))
      ,@body)))

(defun maybe-rename-lexical-var (x symbols-in-bindings)
  (when (or (member x *enclosing-lexicals*)
            (member x *enclosing-function-arguments*)
            (when (boundp '*used-up-names*)
              (member x *used-up-names*))
            (lookup-macro-def x *symbol-macro-env*)
            (member x symbols-in-bindings))
    (clvba-gensym (symbol-name x))))

(defun with-lambda-scope (body)
 (prog1 `((lambda () ,body))
   (setf *vars-needing-to-be-declared* ())))

(define-expression-operator let (bindings &body body)
  (with-declaration-effects (body body)
    (flet ((rename (x) (first x))
           (var (x) (second x))
           (val (x) (third x)))
      (let* ((new-lexicals ())
             (normalized-bindings
              (mapcar (lambda (x)
                        (if (symbolp x)
                            (list x nil)
                            (list (car x) (clvba-macroexpand (cadr x)))))
                      bindings))
             (symbols-in-bindings
              (mapcan (lambda (x) (flatten (cadr x)))
                      normalized-bindings))
             (lexical-bindings
              (loop for x in normalized-bindings
                    unless (special-variable? (car x)) collect
                    (cons (aif (maybe-rename-lexical-var (car x)
                                                         symbols-in-bindings)
                               it
                               (progn
                                 (push (car x) new-lexicals)
                                 (when (boundp '*used-up-names*)
                                   (push (car x) *used-up-names*))
                                 nil))
                          x)))
             (dynamic-bindings
              (loop for x in normalized-bindings
                    when (special-variable? (car x)) collect
                    (cons (clvba-gensym (format nil "~A_~A" (car x) 'tmp-stack))
                          x)))
             (renamed-body
              `(symbol-macrolet ,(loop for x in lexical-bindings
                                       when (rename x) collect
                                       `(,(var x) ,(rename x)))
                 ,@body))
             (*enclosing-lexicals*
              (append new-lexicals *enclosing-lexicals*))
             (*loop-scope-lexicals*
              (when in-loop-scope?
                (append new-lexicals *loop-scope-lexicals*)))
             (let-body
              `(progn
                 ,@(mapcar (lambda (x)
                             `(var ,(or (rename x) (var x)) ,(val x)))
                           lexical-bindings)
                 ,(if dynamic-bindings
                      `(progn
                         ,@(mapcar (lambda (x) `(var ,(rename x)))
                                   dynamic-bindings)
                         (progn
                           (setf ,@(loop for x in dynamic-bindings append
                                        `(,(rename x) ,(var x)
                                           ,(var x) ,(val x))))
                           ,renamed-body))
                 renamed-body))))
        (clvba-compile (cond (in-function-scope? let-body)
                          ;; HACK
                          ((find-if (lambda (x)
                                      (member x '(defun% defvar)))
                                    (flatten
                                     (loop for x in body collecting
                                          (or (ignore-errors (clvba-macroexpand x))
                                              x))))
                           let-body)
                          ;; (t (with-lambda-scope let-body))))))))
                          (t let-body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iteration

(defun make-for-vars/inits (init-forms)
  (mapcar (lambda (x)
            (cons (clvba-macroexpand (if (atom x) x (first x)))
                  (compile-expression (if (atom x) nil (second x)))))
          init-forms))

(defun compile-loop-body (loop-vars body)
  (let* ((in-loop-scope?                                                    t)
         ;; provides lexical bindings for all free variables using WITH
         (in-function-scope?                                                t)
         (*loop-scope-lexicals*                                     loop-vars)
         (*loop-scope-lexicals-captured*                                   ())
         (*clvba-gensym-counter*                             *clvba-gensym-counter*)
         (compiled-body                   (compile-statement `(progn ,@body))))
    ;; the sort is there to make order for output-tests consistent across implementations
    (aif (sort (remove-duplicates *loop-scope-lexicals-captured*)
               #'string< :key #'symbol-name)
         `(clvba-vba:block
              (clvba-vba:with
                  ,(compile-expression
                    `(create
                      ,@(loop for x in it
                              collect x
                              collect (when (member x loop-vars) x))))
                ,compiled-body))
         compiled-body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalutation

(define-expression-operator quote (x)
  (flet ((quote% (expr) (when expr `',expr)))
    (compile-expression
     (typecase x
       (cons `(array ,@(mapcar #'quote% x)))
       (null '(array))
       (keyword x)
       (symbol (symbol-to-vba-string x))
       (number x)
       (string x)
       (vector `(array ,@(loop for el across x collect (quote% el))))))))

(define-expression-operator eval-when (situation-list &body body)
  "The body is evaluated only during the given situations. The
accepted situations are :load-toplevel, :compile-toplevel,
and :execute. The code in BODY is assumed to be Common Lisp code
in :compile-toplevel and :load-toplevel sitations, and CLVBA
code in :execute."
  (when (and (member :compile-toplevel situation-list)
         (member *compilation-level* '(:toplevel :inside-toplevel-form)))
    (eval `(progn ,@body)))
  (if (member :execute situation-list)
      (clvba-compile `(progn ,@body))
      (clvba-compile `(progn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-statement-operator label (label)
  `(clvba-vba:label ,label))

(define-statement-operator on-error-goto (label)
  `(clvba-vba:on-error-goto ,label))

(define-statement-operator on-error-resume-next ()
  `(clvba-vba:on-error-goto))

(define-statement-operator on-error-goto-0 ()
  `(clvba-vba:on-error-goto 0))


(define-statement-operator exit (label)
  `(clvba-vba:exit ,label))


(define-statement-operator type (varname scope &rest elements)
  `(clvba-vba:type ,varname ,scope ,@elements))


(define-statement-operator defsub (name lambda-list &rest body)
  `(clvba-vba:defsub ,name
       ,(loop for arg in lambda-list
           ;; do (format t "Arg is ~a~%" arg)
           collect
             (if (symbolp arg) arg
                 `(clvba-vba:arg ,@arg)))
     nil
     ,(clvba-compile `(progn ,@body))))


(define-statement-operator defun (name lambda-list &rest body)
  `(clvba-vba:defun ,name
       ,(loop for arg in lambda-list
           ;; do (format t "Arg is ~a~%" arg)
           collect
             (if (symbolp arg) arg
                 `(clvba-vba:arg ,@arg)))
     nil
     ,(clvba-compile `(progn ,@body))))
