(in-package #:clvba)

;;; Break something like a lambda list (but not necessarily actually a
;;; lambda list, e.g. the representation of argument types which is
;;; used within an FTYPE specification) into its component parts. We
;;; return twelve values:
;;;  1. a list of the required args;
;;;  2. a list of the &OPTIONAL arg specs;
;;;  3. true if a &REST arg was specified;
;;;  4. the &REST arg;
;;;  5. true if &KEY args are present;
;;;  6. a list of the &KEY arg specs;
;;;  7. true if &ALLOW-OTHER-KEYS was specified.;
;;;  8. true if any &AUX is present (new in SBCL vs. CMU CL);
;;;  9. a list of the &AUX specifiers;
;;; 10. true if a &MORE arg was specified;
;;; 11. the &MORE context var;
;;; 12. the &MORE count var;
;;; 13. true if any lambda list keyword is present (only for
;;;     PARSE-LAMBDA-LIST-LIKE-THING).
;;; 14. the &KEY-OBJECT var
;;;
;;; The top level lambda list syntax is checked for validity, but the
;;; arg specifiers are just passed through untouched.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-list-expander (n-value n-tail forms)
    (let ((n-res (gensym)))
      `(progn
        ,@(mapcar (lambda (form)
                    `(let ((,n-res (cons ,form nil)))
                      (cond (,n-tail
                             (setf (cdr ,n-tail) ,n-res)
                             (setq ,n-tail ,n-res))
                            (t
                             (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                  forms)
        ,n-value))))

(defmacro collect (collections &body body)
  (let ((macros ())
        (binds ()))
    (dolist (spec collections)
      ;;(unless (proper-list-of-length-p spec 1 3)
      ;;  (error "malformed collection specifier: ~S" spec))
      (let* ((name (first spec))
             (default (second spec))
             (kind (or (third spec) 'collect))
             (n-value (gensym (concatenate 'string
                                           (symbol-name name)
                                           "-N-VALUE-"))))
        (push `(,n-value ,default) binds)
        (if (eq kind 'collect)
            (let ((n-tail (gensym (concatenate 'string
                                               (symbol-name name)
                                               "-N-TAIL-"))))
              (if default
                  (push `(,n-tail (last ,n-value)) binds)
                  (push n-tail binds))
              (push `(,name (&rest args)
                            (collect-list-expander ',n-value ',n-tail args))
                    macros))
            (push `(,name (&rest args)
                          (collect-normal-expander ',n-value ',kind args))
                  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))

(defparameter *lambda-list-keywords*
 '(&allow-other-keys &aux &body &environment &key &key-object &optional &rest &whole))

(defun style-warn (&rest args) (apply #'format t args))

(defun parse-lambda-list-like-thing (list)
 (collect ((required)
            (optional)
            (keys)
            (aux))
    (let ((restp nil)
          (rest nil)
          (morep nil)
          (more-context nil)
          (more-count nil)
          (keyp nil)
          (auxp nil)
          (allowp nil)
          (key-object nil)
          (state :required))
      (declare (type (member :allow-other-keys :aux
                             :key
                             :more-context :more-count
                             :optional
                             :post-more :post-rest
                             :required :rest
                             :key-object :post-key)
                     state))
      (dolist (arg list)
        (if (member arg *lambda-list-keywords*)
            (case arg
              (&optional
               (unless (eq state :required)
                 (format t "misplaced &OPTIONAL in lambda list: ~S"
                         list))
               (setq state :optional))
              (&rest
               (unless (member state '(:required :optional))
                 (format t "misplaced &REST in lambda list: ~S" list))
               (setq state :rest))
              (t (format t "unknown LAMBDA-LIST-KEYWORD in lambda list: ~S." arg)))
            (progn
              (when (symbolp arg)
                (let ((name (symbol-name arg)))
                  (when (and (plusp (length name))
                             (char= (char name 0) #\&))
                    (style-warn
                     "suspicious variable in lambda list: ~S." arg))))
              (case state
                (:required (required arg))
                (:optional (optional arg))
                (:rest
                 (setq restp t
                       rest arg
                       state :post-rest))
                (t
                 (format t "found garbage in lambda list when expecting ~
                                  a keyword: ~S"
                                 arg))))))
      (when (eq state :rest)
        (format t "&REST without rest variable"))

      (values (required) (optional) restp rest keyp (keys) allowp auxp (aux)
              morep more-context more-count
              (not (eq state :required))
              key-object))))

;;; like PARSE-LAMBDA-LIST-LIKE-THING, except our LAMBDA-LIST argument
;;; really *is* a lambda list, not just a "lambda-list-like thing", so
;;; can barf on things which're illegal as arguments in lambda lists
;;; even if they could conceivably be legal in not-quite-a-lambda-list
;;; weirdosities
(defun parse-lambda-list (lambda-list)
  ;; Classify parameters without checking their validity individually.
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux
                        morep more-context more-count beyond-requireds? key-object)
      (parse-lambda-list-like-thing lambda-list)
    (declare (ignore beyond-requireds?))
    ;; Check validity of parameters.
    (flet ((need-symbol (x why)
             (unless (symbolp x)
               (format t "~A is not a symbol: ~S" why x))))
      (dolist (i required)
        (need-symbol i "Required argument"))
      (dolist (i optional)
        (typecase i
          (symbol)
          (cons
           (destructuring-bind (var &optional init-form supplied-p) i
             (declare (ignore init-form supplied-p))
             (need-symbol var "&OPTIONAL parameter name")))
          (t
           (format t "&OPTIONAL parameter is not a symbol or cons: ~S"
                           i))))
      (when restp
        (need-symbol rest "&REST argument")))
    ;; Voila.
    (values required optional restp rest keyp keys allowp auxp aux
            morep more-context more-count key-object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lambda lists

(defun parse-optional-spec (spec)
  "Parses an &optional parameter.  Returns 3 values: var, init-form, supplied-p-var.
[&optional {var | (var [init-form [supplied-p-parameter]])}*] "
  (let* ((var (cond ((symbolp spec) spec)
                    ((and (listp spec) (first spec)))))
         (init-form (if (listp spec) (second spec)))
         (supplied-p-var (if (listp spec) (third spec))))
    (values var init-form supplied-p-var)))

(defun parse-extended-function (lambda-list body)
  "The lambda list is transformed as follows:

* standard and optional variables are the mapped directly into
  the vba-lambda list

* keyword variables are not included in the vba-lambda list, but
  instead are obtained from the magic js ARGUMENTS
  pseudo-array. Code assigning values to keyword vars is
  prepended to the body of the function."
  (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux?
                                  aux more? more-context more-count key-object)
      (parse-lambda-list lambda-list)
    (declare (ignore allow? aux? aux more? more-context more-count key-object))
    (let* ( ;; optionals are of form (var default-value)
           (effective-args
            (remove-if #'null
                       (append requireds
                               (mapcar #'parse-optional-spec optionals))))
           (opt-forms
            (mapcar (lambda (opt-spec)
                      (multiple-value-bind (name value suppl)
                          (parse-optional-spec opt-spec)
                        (if suppl
                             `(progn
                               (var ,suppl)
                               ,@(when value
                                   `((when (not ,suppl) (setf ,name ,value)))))
                            (when value
                                 `(setf ,name ,value)))))
                    optionals))
           (rest-form
            (when rest?
              (with-clvba-gensyms (i)
                `(progn (var ,rest (array))
                        (dotimes (,i (- (getprop arguments 'length)
                                        ,(length effective-args)))
                          (setf (aref ,rest
                                      ,i)
                                (aref arguments
                                      (+ ,i ,(length effective-args)))))))))
           (docstring (and (cdr body) (stringp (car body)) (car body)))
           (effective-body (append opt-forms
                                   (awhen rest-form (list it))
                                   (if docstring (rest body) body))))
      (values effective-args effective-body docstring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common

(defun compile-function-body (args body)
  (declaim (optimize (debug 3) (speed 0) (space 0)))
  (compile-statement `(progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lambda

(define-expression-operator lambda (lambda-list &rest body)
  (multiple-value-bind (effective-args effective-body)
      (parse-extended-function lambda-list body)
    `(clvba-vba:lambda ,effective-args
       ,(let ((*function-block-names* ()))
             (compile-function-body effective-args effective-body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; named functions

(defun compile-named-function-body (name lambda-list body)
  (let ((*enclosing-lexicals* (cons name *enclosing-lexicals*))
        (*function-block-names* (list name)))
    (multiple-value-bind (effective-args effective-body docstring)
        (parse-extended-function lambda-list body)
      (values effective-args
              (compile-function-body effective-args effective-body)
              docstring))))


(defun maybe-rename-local-function (fun-name)
  (or (getf *local-function-names* fun-name) fun-name))

(defun collect-function-names (fn-defs)
  (loop for (fn-name) in fn-defs
        collect fn-name
        collect (if (or (member fn-name *enclosing-lexicals*)
                        (lookup-macro-def fn-name *symbol-macro-env*))
                    (clvba-gensym (string fn-name))
                    fn-name)))

(defmacro local-functions (special-op &body bindings)
  `(if in-function-scope?
       (let* ((fn-renames (collect-function-names fn-defs))
              ,@bindings)
         `(,(if compile-expression? 'clvba-vba:|,| 'clvba-vba:block)
            ,@definitions
            ,@(compile-progn body)))
       (clvba-compile (with-lambda-scope `(,',special-op ,fn-defs ,@body)))))


(define-expression-operator function (fn-name)
  ;; one of the things responsible for function namespace
  (clvba-compile (maybe-rename-local-function fn-name)))
