(in-package #:clvba)
(in-readtable :clvba)

(defvar *clvba-print-pretty* t)
(defvar *indent-num-spaces* 4)
(defvar *vba-string-delimiter* #\")

(defvar *indent-level*)
(defvar *column*)

(defvar *clvbaw-stream*)

(defvar %printer-toplevel?)

(defparameter *vba-lisp-escaped-chars*
  '(("\"\"" . #\')
    (#\b . #\Backspace)
    (#\f . #.(code-char 12))
    (#\r . #\Return)))

(defun cl-vba-print (form immediate?)
  (declare (special immediate?))
  (let ((*indent-level* 0)
        (*column* 0)
        (*clvbaw-stream* (if immediate?
                          *clvbaw-stream*
                          (make-string-output-stream)))
        (%clvbaw-accumulator ())
        (%printer-toplevel? t))
    (declare (special %clvbaw-accumulator))
    (with-standard-io-syntax
      (if (and (listp form) (eq 'clvba-vba:block (car form))) ; ignore top-level block
          (loop for (statement . remaining) on (cdr form) do
               (clvba-print statement) 
               (when remaining (clvbaw #\Newline)))
          (clvba-print form)))
    (unless immediate?
      (reverse (cons (get-output-stream-string *clvbaw-stream*)
                     %clvbaw-accumulator)))))


(defun clvbaw (&rest objs)
  (dolist (obj objs)
    (declare (special %clvbaw-accumulator immediate?))
    (typecase obj
      (string
       (incf *column* (length obj))
       (write-string obj *clvbaw-stream*))
      (character
       (if (eql obj #\Newline)
           (setf *column* 0)
           (incf *column*))
       (write-char obj *clvbaw-stream*))
      (otherwise
       (if immediate?
           (let ((str (eval obj)))
             (incf *column* (length str))
             (write-string str *clvbaw-stream*))
           (setf %clvbaw-accumulator
                 (list* obj
                        (get-output-stream-string *clvbaw-stream*)
                        %clvbaw-accumulator)))))))

(defgeneric clvba-print (form))
(defgeneric clvba-print% (vba-primitive args))

(defmethod clvba-print :after (form)
  (declare (ignore form))
  (setf %printer-toplevel? nil))

(defmacro defprinter (vba-primitive args &body body)
  (if (listp vba-primitive)
      (cons 'progn (mapcar (lambda (p)
                             `(defprinter ,p ,args ,@body))
                           vba-primitive))
      (let ((pargs (gensym)))
        `(defmethod clvba-print% ((op (eql ',vba-primitive)) ,pargs)
           (declare (ignorable op))
           (destructuring-bind ,args
               ,pargs
             ,@(loop for x in body collect
                    (if (or (characterp x)
                            (stringp x))
                        (list 'clvbaw x)
                        x)))))))

(defmethod clvba-print ((x null))
  (clvbaw "Null"))

(defmethod clvba-print ((x (eql t)))
  (clvbaw "True"))

(defmethod clvba-print ((x (eql 'clvba-vba:false)))
  (clvbaw "False"))

(defmethod clvba-print ((s symbol))
  (if (keywordp s)
      (clvba-print (string-downcase s))
      (clvbaw (symbol-to-vba-string s))))

(defmethod clvba-print ((char character))
  (clvba-print (string char)))

(defmethod clvba-print ((string string))
  (flet ((lisp-special-char-to-vba (lisp-char)
           (car (rassoc lisp-char *vba-lisp-escaped-chars*))))
    (clvbaw *vba-string-delimiter*)
    (loop for char across string
          for code = (char-code char)
          for special = (lisp-special-char-to-vba char)
          do (cond (special (clvbaw special))
                   ;; ((or (<= code #x1f) (>= code #x80))
                   (t (clvbaw char))))
    (clvbaw *vba-string-delimiter*)
))

(defmethod clvba-print ((number number))
  (format *clvbaw-stream* (if (integerp number) "~D" "~F") number))


(defmethod clvba-print ((compiled-form cons))
  (clvba-print% (car compiled-form) (cdr compiled-form)))


(defun newline-and-indent (&optional indent-spaces)
  (if *clvba-print-pretty*
      (progn (clvbaw #\Newline)
             (loop repeat (if indent-spaces
                              indent-spaces
                              (* *indent-level* *indent-num-spaces*))
                   do (clvbaw #\Space)))
      (clvbaw #\Space)))

(defun print-comment (comment-str)
  (when *clvba-print-pretty*
    (let ((lines (cl-ppcre:split #\Newline comment-str)))
      (dolist (x lines) (clvbaw "' " x) (newline-and-indent)))))



(defvar %equality-ops '(clvba-vba:<> clvba-vba:=))

(let ((precedence-table (make-hash-table :test 'eq)))
  (loop for level in `((clvba-vba:getprop clvba-vba:aref clvba-vba:funcall clvba-vba:subcall)
                       ;; Todo (clvba-vba:^)
                       (clvba-vba:negate)
                       (clvba-vba:* clvba-vba:/)
                       (clvba-vba:mod)
                       (clvba-vba:+ clvba-vba:-)
                       (clvba-vba:&)
                       (clvba-vba:is)
                       (clvba-vba:= clvba-vba:<> clvba-vba:< clvba-vba:> clvba-vba:<= clvba-vba:>=) ;; Todo clvba-vba:like clvba-vba:is
                       (clvba-vba:not)
                       (clvba-vba:and)
                       (clvba-vba:or)
                       ;; Todo (clvba-vba:xor)
                       ;; Todo (clvba-vba:eqv)
                       ;; Todo (clvba-vba:imp)
                       (clvba-vba:|,|)
                       )
     for i from 0
     do (mapc (lambda (symbol)
                (setf (gethash symbol precedence-table) i))
              level))
  (defun precedence (op)
    (gethash op precedence-table -1)))

(defun associative? (op)
  (member op '(clvba-vba:* clvba-vba:and clvba-vba:or
               clvba-vba:funcall clvba-vba:aref clvba-vba:getprop))) ;; these aren't really associative, but rpn

(defun parenthesize-print (x)
  (clvbaw #\() (if (functionp x) (funcall x) (clvba-print x)) (clvbaw #\)))

(defun parenthesize-at-toplevel (x)
  (if %printer-toplevel?
      (parenthesize-print x)
      (funcall x)))

(defun print-op-argument (op argument)
  (setf %printer-toplevel? nil)
  (let ((arg-op (when (listp argument) (car argument))))
    (if (or (< (precedence op) (precedence arg-op))
            (and (= (precedence op) (precedence arg-op))
                 (or (not (associative? op)) (not (associative? arg-op)))))
        (parenthesize-print argument)
        (clvba-print argument))))

(defun print-op (op)
  (clvbaw (string-capitalize op)))

(defprinter clvba-vba:not (x)
  "Not "(print-op-argument op x))
    

(defprinter clvba-vba:negate (x)
  "-"(print-op-argument op x))

(defprinter (clvba-vba:return) (&optional (x nil x?))
  (when x?
    (print-op-argument op x)))

(defprinter (clvba-vba:+ clvba-vba:- clvba-vba:* clvba-vba:/ clvba-vba:mod clvba-vba:and clvba-vba:or clvba-vba:= 
                     clvba-vba:> clvba-vba:>= clvba-vba:< clvba-vba:<= clvba-vba:& clvba-vba:is)
    (&rest args)
  (loop for (arg . remaining) on args do
       (print-op-argument op arg)
       (when remaining (format *clvbaw-stream* " ~(~A~) " op))))

(defprinter clvba-vba:|:=| (var-name var-value)
            (clvba-print var-name)" := "(clvba-print var-value))


;; (defprinter clvba-vba:& (&rest args)
;;   (loop for (arg . remaining) on args
;;   do
;;     ;; (format t "~a: ~a~%" (type-of arg) arg)
;;     (if (listp arg)
;;         (parenthesize-print arg)
;;         (clvba-print arg))
;;        (when remaining (format *clvbaw-stream* " ~(~A~) " op))))

(defprinter (clvba-vba:<> clvba-vba:=) (x y)
  (flet ((parenthesize-equality (form)
           (if (and (consp form) (member (car form) %equality-ops))
               (parenthesize-print form)
               (print-op-argument op form))))
    (parenthesize-equality x)
    (format *clvbaw-stream* " ~A " op)
    (parenthesize-equality y)))

(defprinter clvba-vba:aref (array &rest indices)
  (print-op-argument 'clvba-vba:aref array)
  (dolist (idx indices)
    (clvbaw #\() (clvba-print idx) (clvbaw #\))))

(defun print-comma-delimited-list (clvba-forms)
  (loop for (form . remaining) on clvba-forms do
        (print-op-argument 'clvba-vba:|,| form)
        (when remaining (clvbaw ", "))))


(defprinter (clvba-vba:|,|) (&rest expressions)
  (print-comma-delimited-list expressions))

(defprinter clvba-vba:funcall (fun-designator &rest args)
  (print-op-argument op fun-designator)"("(print-comma-delimited-list args)")")

(defprinter clvba-vba:subcall (sub-designator &rest args)
  (clvba-print sub-designator)" "(print-comma-delimited-list args))

(defprinter clvba-vba:block (&rest statements)
  (incf *indent-level*)
  (dolist (statement statements)
    (newline-and-indent)
    (clvba-print statement))
  (decf *indent-level*)
  (newline-and-indent))

(defun print-fun/sub-def (fun-or-sub name args body)
  (newline-and-indent)
  (clvbaw (string-capitalize fun-or-sub))
  (format *clvbaw-stream* " ~:[~;~A~](" name (symbol-to-vba-string name))
  (loop for (arg . remaining) on args do
       (if (symbolp arg)
           (clvbaw (symbol-to-vba-string arg))
           (clvba-print arg))
       (when remaining (clvbaw ", ")))
  (clvbaw ") ")
  (clvba-print body)
  (decf *indent-level*)
  (clvbaw "End ")
  (clvbaw (string-capitalize fun-or-sub))
  (newline-and-indent))

(defprinter clvba-vba:defun (name args docstring body-block)
  (when docstring (print-comment docstring))
  (print-fun/sub-def "Function" name args body-block))

(defun print-fun-def (name args body)
  ;; (format *clvbaw-stream* "function ~:[~;~A~](" name (symbol-to-vba-string name))
  (loop for (arg . remaining) on args do
        (clvbaw (symbol-to-vba-string arg)) (when remaining (clvbaw ", ")))
  ;; (clvbaw ") ")
  (clvba-print body))


(defprinter clvba-vba:defsub (name args docstring body-block)
  (when docstring (print-comment docstring))
  (print-fun/sub-def "Sub" name args body-block))

(defprinter clvba-vba:getprop (obj slot)
  (print-op-argument op obj)"."(clvbaw (symbol-to-vba-string slot)))

(defprinter clvba-vba:if (test consequent &rest clauses)
  "If " (clvba-print test) " Then"
  (clvba-print consequent)
  (loop while clauses do
       (ecase (car clauses)
         (:else-if (incf *indent-level*)
                   (clvbaw "ElseIf ") (clvba-print (cadr clauses)) (clvbaw " Then")
                   (decf *indent-level*)
                   (clvba-print (caddr clauses))
                   (setf clauses (cdddr clauses)))
         (:else (clvbaw "Else")
                (clvba-print (cadr clauses))
                (return))))
  (clvbaw "End If"))


;; Set objectvar = {[New] objectexpression | Nothing}
(defprinter clvba-vba:set (var-name var-value)
  "Set "(clvba-print var-name)" = "(clvba-print var-value))

(defprinter clvba-vba:dim (var-name &key type documentation scope)
  (when documentation (print-comment documentation))
  (when scope
    (print-op scope)
    (clvbaw " "))
  (cond ((symbolp var-name)
         (clvbaw (symbol-to-vba-string var-name)))
        ((listp var-name)
         (if (cadr var-name)
             (progn
               (format *clvbaw-stream* "~a(" (car var-name))
               (print-comma-delimited-list (cdr var-name))
               (format *clvbaw-stream* ")"))
             (format *clvbaw-stream* "~a()" (car var-name)))))
  (when type (clvbaw " As ") (clvba-print type)))


(defprinter clvba-vba:redim (var-name &key type preserve)
  (clvbaw "Redim ")
  (when preserve (clvbaw "Preserve "))
  (cond ((symbolp var-name)
         (clvbaw (symbol-to-vba-string var-name)))
        ((listp var-name)
         (if (cadr var-name)
             (progn
               (format *clvbaw-stream* "~a(" (car var-name))
               (print-comma-delimited-list (cdr var-name))
               (format *clvbaw-stream* ")"))
             (format *clvbaw-stream* "~a()" (car var-name)))))
  (when type (clvbaw " As ") (clvba-print type)))


(defprinter clvba-vba:label (label)
  (clvba-print label)":"
  (incf *indent-level*))

(defprinter clvba-vba:on-error-goto (label)
  "On Error Goto "(clvba-print label))

(defprinter clvba-vba:on-error-resume-next ()
  "On Error Resume Next")

(defprinter clvba-vba:exit (label)
  "Exit "(clvba-print label))


;;; iteration
;; For counter = start To end [Step step]
;; [statements]
;; [Exit For]
;; [statements]
;; Next [counter]
(defprinter clvba-vba:for (var start end step body-block)
  "For "(clvba-print var)" = "(clvba-print start)" To "(clvba-print end)
  (if step 
      (progn
        (clvbaw " Step ") (clvba-print step)))
  (clvba-print body-block)
  "Next "(clvba-print var))

(defprinter clvba-vba:for-in (var object body-block)
  "For Each "(clvba-print var)" In "(clvba-print object)
  (incf *indent-level*) 
  (clvba-print body-block)
  (decf *indent-level*)
  "Next "(clvba-print var))

;; While condition
;; [statements]
;; Wend
(defprinter clvba-vba:while (expression body-block)
  (print-op op)" "(clvba-print expression)
  (clvba-print body-block)
  (decf *indent-level*)
  (clvbaw "Wend"))

;; With object
;; [statements]
;; End With
(defprinter clvba-vba:with (expression body-block)
  (print-op op)" "(clvba-print expression)
  (clvba-print body-block)
  ;; (decf *indent-level*)
  (clvbaw "End With"))

;; Select Case testexpression
;; [Case expressionlist-n
;; [statements-n]] 
;; [Case Else
;; [elsestatements]]
;; End Select
(defprinter clvba-vba:select-case (test &rest clauses)
  "Select Case "(clvba-print test)
  (incf *indent-level*)
  ;; (newline-and-indent)
  (flet ((print-body-statements (body-statements)
           (incf *indent-level*)
           (loop for statement in body-statements do
                (progn (newline-and-indent)
                       (clvba-print statement)))
           (decf *indent-level*)))
    (loop for (val . statements) in clauses
       do (progn (newline-and-indent)
                 (if (eq val 'clvba-vba:default)
                     (progn (clvbaw "Case Else")
                            (print-body-statements statements))
                     (progn (clvbaw "Case ") (clvba-print val)
                            (print-body-statements statements))))))
  (decf *indent-level*)
  (newline-and-indent)
  "End Select")

(defprinter clvba-vba:escape (literal-vba)
  (clvbaw literal-vba))

(defprinter clvba-vba:raw (x)
  (format *clvbaw-stream* "~a" x))


(defprinter clvba-vba:lambda (args body-block)
  ;; (parenthesize-at-toplevel
  ;;  (lambda () (print-fun-def nil args body-block))))
  (print-fun-def nil args body-block))


(defprinter clvba-vba:type (varname scope &rest elements)
  (print-op scope)" "
   "Type "(clvba-print varname)
  (incf *indent-level*)
  ;; (newline-and-indent)
  (loop for (element-name . element-type) in elements
     do (progn (newline-and-indent)
               (clvba-print element-name)
               (clvbaw " As ")
               (clvba-print element-type)))
  (decf *indent-level*)
  (newline-and-indent)
  "End Type")


(defprinter clvba-vba:to (x y)
  (clvba-print x)" To "(clvba-print y))


(defprinter clvba-vba:arg (var-name &key optional by paramarray type defaultvalue)
  (when optional (clvbaw "Optional "))
  (when by (clvbaw "ByValue "))
  (when paramarray (clvbaw "ParamArray "))
  (cond ((symbolp var-name)
         (clvbaw (symbol-to-vba-string var-name)))
        ((listp var-name)
         (if (cadr var-name)
             (progn
               (format *clvbaw-stream* "~a(" (car var-name))
               (print-comma-delimited-list (cdr var-name))
               (format *clvbaw-stream* ")"))
             (format *clvbaw-stream* "~a()" (car var-name)))))
  (when type (clvbaw " As ") (clvba-print type))
  (when defaultvalue
    (clvbaw " = ")
    (clvba-print defaultvalue)))
