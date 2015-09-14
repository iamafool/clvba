(in-package #:clvba)
(in-readtable :clvba)

(macrolet ((define-trivial-mappings (&rest mappings)
             `(progn
                ,@(loop for (macro-name clvba-op) on mappings by #'cddr collect
                       `(defclvbamacro ,macro-name (&rest args)
                          (cons ',clvba-op args))))))
  (define-trivial-mappings
    string= eql
    eq      eql
    =       eql))

(defclvbamacro null (x)
  `(equal ,x nil))

;;; Math

(defmacro def-vba-maths (&rest mathdefs)
  `(progn ,@(mapcar (lambda (def) (cons 'defclvbamacro def)) mathdefs)))

(def-vba-maths
    (1+ (n) `(+ ,n 1))
    (1- (n) `(- ,n 1)))


(defclvbamacro var (var-name &optional (value (values) value?) docstring)
  `(progn
     (dim ,var-name :as Variant)
     ,(when value?
            `(setf ,var-name ,value))))

;;; Data structures

(defclvbamacro array (&rest initial-values)
  `(*array ,@initial-values))

(defclvbamacro length (a)
  `(getprop ,a 'length))


;;; conditionals
(defclvbamacro case (value &rest clauses)
  (let ((allowed-symbols '(t otherwise false %true)))
   (labels ((make-switch-clause (val body more)
              (cond ((listp val)
                     (append (mapcar #'list (butlast val))
                             (make-switch-clause
                              (if (eq t (car (last val))) ;; literal 'true'
                                  '%true
                                  (car (last val)))
                              body
                              more)))
                    ((and (symbolp val)
                          (symbolp (clvba-macroexpand-1 val))
                          (not (keywordp val))
                          (not (member val allowed-symbols)))
                     (error "CLVBA only supports keywords, numbers, and string literals as keys in case clauses. ~S is a symbol in clauses ~S"
                            val clauses))
                    (t
                     `((,(case val
                           ((t otherwise) 'default)
                           (%true          t)
                           (t              (clvba-macroexpand-1 val)))
                         ,@body))))))
     `(switch ,value ,@(mapcon (lambda (clause)
                                 (make-switch-clause (car (first clause))
                                                     (cdr (first clause))
                                                     (rest clause)))
                               clauses)))))

(defclvbamacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defclvbamacro unless (test &rest body)
  `(when (not ,test) ,@body))


;;; defining setf expanders

(defvar *defun-setf-name-prefix* 'setf_)

(defclvbamacro defun-setf (name lambda-list &body body)
  (let ((mangled-function-name
         (intern (format nil "~A~A" (string *defun-setf-name-prefix*) (string name))
                 (symbol-package name))))
    (setf (gethash name *setf-expanders*)
          (lambda (access-args store-form)
            `(,mangled-function-name ,store-form ,@access-args)))
    `(defun ,mangled-function-name ,lambda-list ,@body)))



;;; setf

(defclvbamacro setf (&rest args)
  (assert (evenp (length args)) ()
          "~s does not have an even number of arguments." `(setf ,args))
  `(progn ,@(loop for (place value) on args by #'cddr collect
                 (aif (and (listp place) (gethash (car place) *setf-expanders*))
                      (funcall it (cdr place) value)
                      `(clvba-assign ,place ,value)))))

(defclvbamacro psetf (&rest args)
  (let ((places (loop for x in args by #'cddr collect x))
        (vals (loop for x in (cdr args) by #'cddr collect x)))
    (let ((gensyms (loop repeat (length places) collect (clvba-gensym))))
      `(let ,(mapcar #'list gensyms vals)
         (setf ,@(mapcan #'list places gensyms))))))

(defun check-setq-args (args)
  (let ((vars (loop for x in args by #'cddr collect x)))
    (let ((non-var (find-if (complement #'symbolp) vars)))
      (when non-var
        (error 'type-error :datum non-var :expected-type 'symbol)))))

(defclvbamacro setq (&rest args)
  (check-setq-args args)
  `(setf ,@args))

(defclvbamacro psetq (&rest args)
  (check-setq-args args)
  `(psetf ,@args))




;;; Concatenation

(defclvbamacro concatenate (result-type &rest sequences)
  (assert (equal result-type ''string) () "Right now CLVBA 'concatenate' only support strings.")
  (cons '& sequences))


;;; Destructuring bind

(defun complex-vba-expr? (expr)
  (consp (if (symbolp expr) (clvba-macroexpand expr) expr)))

(defun hoist-expr? (bindings expr)
  (and (> (length bindings) 1) (complex-vba-expr? expr)))

(defun destructuring-wrap (arr n bindings body)
  (cond ((null bindings) body)
        ((eq (car bindings) '&rest)
         (cond ((and (= (length bindings) 2) (atom (second bindings)))
                `(let ((,(second bindings) (if (> (length ,arr) ,n) ((@ ,arr slice) ,n) '())))
                   ,body))
               (t (error "~a is invalid in destructuring list." bindings))))
        ((eq (car bindings) '&optional)
         (destructuring-wrap arr n (cdr bindings) body))
        (t (let ((var (car bindings))
                 (inner-body (destructuring-wrap arr (1+ n) (cdr bindings) body)))
             (cond ((null var) inner-body)
                   ((atom var) `(let ((,var (aref ,arr ,n))) ,inner-body))
                   (t `(,'destructuring-bind ,var (aref ,arr ,n) ,inner-body)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun dot->rest (x)
    (cond ((atom x) x)
          ((not (listp (cdr x)))        ; dotted list
           (list (dot->rest (car x)) '&rest (dot->rest (cdr x))))
          (t (cons (dot->rest (car x)) (dot->rest (cdr x))))))

  (defun property-bindings-p (x)
    (when (consp x)
      (every (lambda (y)
               (or (keywordp y)
                   (and (consp y)
                        (= (length y) 2)
                        (symbolp (car y))
                        (not (keywordp (car y)))
                        (keywordp (cadr y)))))
             x)))

  (defun extract-bindings (x)
    ;; returns a pair of destructuring bindings and property bindings
    (cond ((atom x) (list x nil))
          ((property-bindings-p x)
           (let ((var (clvba-gensym)))
             (list var (list x var))))
          (t (loop :for y :on x
               :for (d p) = (extract-bindings (car y))
               :append (list d) :into ds
               :when p :append p :into ps
               :finally (return (list ds ps))))))

  (defun property-bindings (bindings expr body)
    `(let ,(loop :for b :in bindings
             :for (var p) = (if (consp b) b (list (intern (string b)) b))
             :collect `(,var (@ ,expr ,p)))
       ,@body)))

(defclvbamacro bind (bindings expr &body body)
  (setf bindings (dot->rest bindings))
  (destructuring-bind (d p)
      (extract-bindings bindings)
    (cond ((and (atom d)
                (or (= (length bindings) 1)
                    (atom expr)
                    (atom (clvba-macroexpand expr))))
           (property-bindings bindings expr body))
          ((atom d)
           (with-clvba-gensyms (var)
             `(let ((,var ,expr))
                (bind ,bindings ,var ,@body))))
          ((null p) `(destructuring-bind ,bindings ,expr ,@body))
          (t `(destructuring-bind ,d ,expr
                (bind* ,p ,@body))))))

(defclvbamacro bind* (bindings &body body)
  (cond ((= (length bindings) 2)
         `(bind ,(car bindings) ,(cadr bindings) ,@body))
        (t `(bind ,(car bindings) ,(cadr bindings)
              (bind* ,(cddr bindings) ,@body)))))

;;; Control structures

(defclvbamacro prog1 (first &rest others)
  (with-clvba-gensyms (val)
    `(let ((,val ,first))
       ,@others
       ,val)))

(defclvbamacro prog2 (first second &rest others)
  `(progn ,first (prog1 ,second ,@others)))


;;; misc

(defclvbamacro let* (bindings &body body)
  (if bindings
      `(let (,(car bindings))
         (let* ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defparameter *table-parameters* nil)

;;; VBA macros

(defmacro clvbas (&body body)
  `(progn
     (clvba-to-stream *clvba-stream*
       ,@body)
     (format *clvba-stream* "~%")))

(defclvbamacro addCC (sub-name &key (tag "") (title "") (CC-type 'wdContentControlRichText))
  "AddCC = Add Content Control."
  `(defsub ,sub-name ()
     (dim i :as integer)
     (setf i (@ (get-CC-tag ,tag) Count))
     (if (= i 0)
         (setf (@ (aref ActiveDocument.Variables (& ,title "_seq") ) Value) 0)
         (setf i (@ (aref ActiveDocument.Variables (& ,title "_seq") ) Value)))

     (if (<> Selection.Range "")
         (progn
           (with Selection.Range
                 (.ContentControls.Add ,CC-type)
                 (setf (@ (.ContentControls 1) LockContentControl) True)
                 ,(if (string/= tag "")
                      `(setf (@ (.ContentControls 1) tag) ,tag))
                 ,(if (string/= title "")
                      `(setf (@ (.ContentControls 1) title) (& ,title (1+ i)))))
           (setf (@ (aref ActiveDocument.Variables (& ,title "_seq") ) Value) (1+ i)))
         (subcall MsgBox "Please select some text."))))


(defclvbamacro cell-split (row column row-no column-no)
  "Split the cell located at (row, column) into row-no rows and column-no columns."
  `(subcall (@ (aref (@ (aref .rows ,row) Cells) ,column) split) ,row-no ,column-no))

(defclvbamacro cell-text (row column text)
  "Set the text of cell located at (row, column)"
  `(setf (@ (aref (@ (aref .rows ,row) Cells) ,column) Range.text) ,text))

(defclvbamacro cell-align (row column alignment)
  "Set the alignment of cell located at (row, column)"
  `(setf (@ (aref (@ (aref .rows ,row) Cells) ,column) Range.ParagraphFormat.alignment) ,alignment))

(defclvbamacro cell-vertical-align (row column alignment)
  "Set the alignment of cell located at (row, column)"
  `(setf (@ (aref (@ (aref .rows ,row) Cells) ,column) VerticalAlignment) ,alignment))

(defclvbamacro left-indent (row column indent)
  "Set the left indent of paragraph located at (row, column)"
  `(setf (@ (aref (@ (aref .rows ,row) Cells) ,column) Range.ParagraphFormat.LeftIndent) ,indent))

(defclvbamacro get-CC-tag (tag)
  "Get the Content Controls by tag"
  `(ActiveDocument.SelectContentControlsByTag ,tag))

(defclvbamacro get-CC-text (title)
  "Get the text of Content Controls by title"
  `(@ ((ActiveDocument.SelectContentControlsByTitle ,title) 1) Range.text))


(defclvbamacro set-cell-width (row column width adjust)
  "Set the width of cell located at (row, column)"
  `(subcall (@ ((@ (.rows ,row) Cells) ,column) SetWidth) ,width ,adjust))

(defclvbamacro set-border-bm (bookmark which style)
    "Set the border located by bookmar"
    `(setf 
     (@ (aref (@ (aref (@ (aref .Range.Bookmarks ,bookmark) Range.rows) 1) Borders) ,which) LineStyle)
     ,style))


(defparameter *layout* nil)

(defun new-legacy (new legacy)
  (if (string= *layout* "NEW") new legacy))
  
;; Type in Cell by Row
(defclvbamacro row-text (rowNumber noColumn text &optional (width01 nil width01-supplied-p))
  `(progn
     (setf rowNumber ,rowNumber)
     (setf arrayLabel (split ,text "|"))
     (cell-split rowNumber 1 2 1)
     (cell-split rowNumber 1 1 ,noColumn)

     ;; set the first column width
     ,(if (and width01-supplied-p width01)
          `(progn
            (for (i 1 ,noColumn)
                 (set-cell-width rowNumber i (Round (* (/ 1 ,noColumn) tableWidth) 2) wdAdjustNone)))
          `(progn
            (set-cell-width rowNumber 1 (Round (* 0.3 tableWidth) 2) wdAdjustNone)
            (for (i 2 ,noColumn)
                 (set-cell-width rowNumber i (Round (* (/ 0.7 (1- ,noColumn)) tableWidth) 2) wdAdjustNone))))

     (for (i 1 (1+ (UBound arrayLabel)))
          (with (@ ((@ (.rows rowNumber) Cells) i) Range)
                (setf .text (newLine (aref arrayLabel (- i 1))))
                (if (<> i 1)
                    (setf .ParagraphFormat.alignment wdAlignParagraphCenter)))
          (setf (@ ((@ (.rows rowNumber) Cells) i) VerticalAlignment)
                wdCellAlignVerticalBottom))))


(defclvbamacro row-text01 (rowNumber noColumn text &key (width01 nil width01-supplied-p) (left-indent 1 li-supplied-p))
  `(progn
     (setf rowNumber ,rowNumber)
     (setf arrayLabel (split ,text "|"))
     (cell-split rowNumber 1 2 1)
     (cell-split rowNumber 1 1 ,noColumn)

     ,(if (and width01-supplied-p (listp width01))
          `(setf arrayWidth (split ,(format nil "~{~a~^|~}" width01) "|")))

     (for (i 1 (1+ (UBound arrayLabel)))
          ,(if (and width01-supplied-p width01)
               (cond
                 ((listp width01)
                  `(set-cell-width rowNumber i (Round (* (aref arrayWidth (- i 1)) tableWidth) 2) wdAdjustNone))
                 ((eq width01 t)
                  `(set-cell-width rowNumber i (Round (* (/ 1 ,noColumn) tableWidth) 2) wdAdjustNone))
                 (t
                  `((progn
                      (if (= i 1 )
                          (set-cell-width rowNumber 1 (Round (* 0.3 tableWidth) 2) wdAdjustNone)
                          (set-cell-width rowNumber i (Round (* (/ 0.7 (1- ,noColumn)) tableWidth) 2) wdAdjustNone)))))))
          
          (with (@ ((@ (.rows rowNumber) Cells) i) Range)
                (setf .text (newLine (aref arrayLabel (- i 1))))
                (if (<> i 1)
                    (setf .ParagraphFormat.alignment wdAlignParagraphCenter)
                    ,(if li-supplied-p
                         `(setf .ParagraphFormat.LeftIndent ,left-indent))))
          (setf (@ ((@ (.rows rowNumber) Cells) i) VerticalAlignment)
                wdCellAlignVerticalBottom))))


;; Define cellNote
(defclvbamacro cell-note (rowNumber columnNumber Note)
  `(progn
     (with (Selection.Tables 1)
           (cell-text ,rowNumber ,columnNumber ,Note)
           (cell-align ,rowNumber ,columnNumber wdAlignParagraphLeft))))


;;; Cell Content Control Note
(defclvbamacro cell-content-control (rowNumber columnNumber split alignment Note)
  `(progn
     (setf cccRowNumber ,rowNumber)
     ,(if (= split 1)
          `(cell-split cccRowNumber ,columnNumber 2 1))

     ,@(loop for x in Note
          collect
            `(progn
               (set objR (@ ((@ (.rows cccRowNumber) Cells) ,columnNumber) Range))
               (subcall objR.SetRange objR.Start (1- objR.End))
               (subcall objR.Collapse wdCollapseEnd)
               (subcall objR.Select)
               ,(if (= (length x) 3)
                    `(progn
                       (set objCC (ActiveDocument.ContentControls.Add wdContentControlRichText objR))
                       (setf objCC.tag ,(first x))
                       (setf objCC.title ,(second x))
                       (setf objCC.Range.text (newLine ,(third x))))
                    `(setf objR.text (newLine ,(first x))))))

     ,(if (> alignment -1)
          `(cell-align cccRowNumber ,columnNumber ,alignment))
     
     (set objR Nothing)
     (set objCC Nothing)))

(defun create-table* (table-parameters column-header footnote body)
  "The structure to create table."
  (let ((studyNo (string-upcase (cdr (assoc "studyNo" table-parameters :test #'string=))))
        ;; (ref (string-upcase (cdr (assoc "ref" table-parameters :test #'string=))))
        (tableNo (cdr (assoc "tableNo" table-parameters :test #'string=)))
        (title (cdr (assoc "title" table-parameters :test #'string=)))
        (analysisSet (cdr (assoc "analysisSet" table-parameters :test #'string=)))
        ;; (noTrtgrp (cdr (assoc "noTrtgrp" table-parameters :test #'string=)))
        (layout (string= "True" (cdr (assoc "layout" table-parameters :test #'string=)))))
    
  `(defsub createTable (tableNo title analysisSet ref noTrtgrp)
    (dim x :as Integer)
    (dim w :as Integer)
    (dim c :as Integer)
    (dim i :as integer)
    (dim row :as Integer)
    (dim noColumns :as Integer)
    (dim noFootnote :as Integer)
    (dim rowIndexTB :as Integer)
    (dim myTable :as Table)
    (dim tempCC :as ContentControl)
    (dim tempMyCC :as myCC)

    ;; Speed up
    (setf Application.ScreenUpdating False)

    ;; How many columns
    (setf noColumns (+ noTrtgrp (oneOrZero totalPresent)))

    ;; Document Variable
    (setf (@ (aref ActiveDocument.Variables (& tableNo "_ref") ) Value) ref)

    ;; Set the paragraph format
    (with Selection.ParagraphFormat
          (setf .LineSpacingRule wdLineSpaceSingle)
          (setf .SpaceAfter 0))

    ;; 道生一一生二二生三三生万物
    (subcall ActiveDocument.Tables.Add
             (|:=| Range Selection.Range)
             (|:=| NumRows 1)
             (|:=| NumColumns 1)
             (|:=| DefaultTableBehavior wdWord9TableBehavior)
             (|:=| AutoFitBehavior wdAutoFitFixed))

    (set myTable (Selection.Tables 1))

    (with myTable
        (setf .Borders.Enable False)
        (setf .Spacing 0)
        (setf .AllowAutoFit False)
        (setf tableWidth (Round (@ (aref (@ (aref .rows 1) Cells) 1) Width) 2))
        (setf .Range.Font.Size 10)

        ,(if layout
             `(progn
               ;; Title
               (cell-content-control .rows.Count 1 1 -1 ((,tableNo "Table_NO" ,tableNo)
                                                         (": ")
                                                         (,tableNo "Title" ,title)
                                                         ("; ")
                                                         (,tableNo "Analysis_Set" ,analysisSet)
                                                         (" Analysis Set ")
                                                         ("(Study ")
                                                         (,tableNo "Study_No" ,studyNo)
                                                         (")")
                                                         ))
               (subcall (@ (aref .rows .rows.Count) Range.Bookmarks.Add) (& tableNo "_" "ref_" (Replace ref "." "_"))))

             `(progn
               ;; Title
               (cell-content-control .rows.Count 1 1 -1 ((,tableNo "Table_NO" ,tableNo) (": ") (,tableNo "Title" ,title)))
               (subcall (@ (aref .rows .rows.Count) Range.Bookmarks.Add) (& tableNo "_" "ref_" (Replace ref "." "_")))

               ;; Analysis Set
               (cell-content-control .rows.Count 1 1 -1 (("(Study ")
                                                         (,tableNo "Study_No" ,studyNo)
                                                         (": ")
                                                         (,tableNo "Analysis_Set" ,analysisSet)
                                                         (" Analysis Set)")))))
               
        
        ;; Column Header
        ,@column-header

        ;; body
        ,@body
        ;; Footnote
        (subcall (@ (aref .rows .rows.Count) Range.Bookmarks.Add) (& tableNo "footnote"))
        ,@footnote

        ;; Get the No of global footnotes
        (setf noFootnote 0)
        (subcall Erase arrayFootnote)

        (if (<> (@ (aref ActiveDocument.Variables "footnote_order") Value) " ")
            (progn
              (dim (arrayfootnoteOrder) :as String)
              (setf arrayfootnoteOrder (split (@ (aref ActiveDocument.Variables "footnote_order") Value)  "|"))

              (for-in (tempCC (get-CC-tag "footnote"))
                      (if (<> (aref arrayfootnoteOrder (1- (ScanNum tempCC.title))) "0")
                          (progn
                            (setf tempMyCC.tag "footnote")
                            (setf tempmyCC.title (aref arrayfootnoteOrder (1- (ScanNum tempCC.title))))
                            (setf tempMyCC.text tempCC.Range.text)
                            (redim (arrayFootnote noFootnote) :preserve t)
                            (setf (aref arrayFootnote noFootnote) tempMyCC)
                            (setf noFootnote (1+ noFootnote)))))

              (if (> noFootnote 0)
                  (progn
                    (subcall SortMyCCByTitle arrayFootnote)
                    (for (i 1 noFootnote)
                         (cell-content-control .rows.Count 1 1 -1 ((,tableNo (& "footnote" (+ i ,(length footnote)))
                                                                             (@ (aref arrayFootnote (1- i)) Text)))))))))

        ;; Define the borders
        (setf .Borders.Enable False)
       
        ,(if layout
             `(progn
                (setf (aref (@ (aref .Rows 1) Borders) wdBorderTop) wdLineStyleSingle)
                (setf (aref (@ (aref .Rows 1) Borders) wdBorderBottom) wdLineStyleSingle)
                (setf (aref (@ (aref .Rows (- .Rows.Count 1)) Borders) wdBorderBottom) wdLineStyleSingle))
             `(progn
                (set-border-bm (& tableNo "treatmentTop") wdBorderTop wdLineStyleSingle)
                (set-border-bm (& tableNo "treatmentBottom") wdBorderBottom wdLineStyleSingle)
                (set-border-bm (& tableNo "footnote") wdBorderTop wdLineStyleSingle)))

        (subcall .AutoFitBehavior wdAutoFitWindow)
        (subcall (@ (aref (@ (aref .Rows 1) Cells) 1) Select))
        (subcall Selection.Collapse))
    
    (setf Application.ScreenUpdating True)
    )))


(defclvbamacro create-label (var caption left top &optional (var-form 'TempForm))
  `(progn
     (dim ,var :as Object)
     (set ,var ((@ ,var-form Designer.Controls.Add) "Forms.Label.1"))
     (with ,var
           (setf .Caption ,caption)
           (setf .Left ,left)
           (setf .Top ,top))))

(defclvbamacro create-button (var name caption left top &optional (var-form 'TempForm))
  `(progn
     (dim ,var :as Object)
     (set ,var ((@ ,var-form Designer.Controls.Add) "Forms.CommandButton.1"))
     (with ,var
           (setf .Name ,name)
           (setf .Caption ,caption)
           (setf .Left ,left)
           (setf .Top ,top))))

(defclvbamacro create-combo-box (var name left top &optional (var-form 'TempForm))
  `(progn
     (dim ,var :as Object)
     (set ,var ((@ ,var-form Designer.Controls.Add) "Forms.ComboBox.1"))
     (with ,var
           (setf .Name ,name)
           (setf .Left ,left)
           (setf .Top ,top))))

(defclvbamacro create-text-box (var name left top &optional (var-form 'TempForm))
  `(progn
     (dim ,var :as Object)
     (set ,var ((@ ,var-form Designer.Controls.Add) "Forms.TextBox.1"))
     (with ,var
           (setf .Name ,name)
           (setf .Left ,left)
           (setf .Top ,top))))

(defclvbamacro combine-column-header (tag title header)
  `(progn
     (dim rowNew :as Row)
     (dim rowOld :as Row)
     (dim rows01 :as Rows)

     (dim celTable :as Cell)
     (dim startColumnNumber :as Integer)
     (dim endColumnNumber :as Integer)
     (dim rowNumber :as Integer)
     (dim tempTable :as Table)
     
     (set tempTable (aref Selection.Tables 1))
     
     (setf rowNumber (aref Selection.Information wdStartOfRangeRowNumber))
     
     (setf startColumnNumber (aref Selection.Information wdStartOfRangeColumnNumber))
     (setf endColumnNumber (aref Selection.Information wdEndOfRangeColumnNumber))

     ;; (if (= startColumnNumber endColumnNumber)
     ;;     (progn
     ;;       (subcall msgbox "Please select at least two continued cells.")
     ;;       (subcall Unload Me)
     ;;       (exit Sub)))
     
     (if (= (aref Selection.Information wdWithInTable) True)
         (progn
           (set rowOld (aref Selection.rows 1))
           (set rows01 Selection.rows)
           (set rowNew (rows01.Add (|:=| BeforeRow (aref Selection.rows 1))))))
     
     (if (<> (aref rowOld.Borders wdBorderTop) wdLineStyleNone)
         (progn
           (setf (aref rowOld.Borders wdBorderTop) wdLineStyleNone)
           (setf (aref rowNew.Borders wdBorderTop) wdLineStyleSingle)))
     
     (for-in (celTable rowNew.Cells)
             (setf (@ (aref celTable.Range.ParagraphFormat.Borders wdBorderBottom) LineStyle) wdLineStyleNone))
     
     (with Selection
           (subcall .SetRange
                    (|:=| Start (@ (tempTable.Cell rowNumber startColumnNumber) Range.Start))
                    (|:=| End (@ (tempTable.Cell rowNumber endColumnNumber) Range.End)))
           (subcall .Cells.Merge))

     (with tempTable
           (setf (@ (aref (@ (aref (@ (aref .Rows rowNumber) cells) startColumnNumber) Range.ParagraphFormat.Borders)
                          wdBorderBottom) LineStyle) wdLineStyleSingle)
           (setf (@ (aref (@ (aref .Rows rowNumber) cells) startColumnNumber) Range.ParagraphFormat.alignment) wdAlignParagraphCenter)
           (cell-content-control rowNumber startColumnNumber 0 -1 ((,tag ,title ,header))))
     ))


