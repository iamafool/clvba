(in-package #:cl)

(pushnew :clvba *features*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :clvba)
    (named-readtables:defreadtable :clvba
      (:merge :standard)
      (:case :invert))))

(named-readtables:in-readtable :clvba)

(defpackage #:clvba
  (:use #:cl #:anaphora #:named-readtables #:port)
  (:nicknames #:vba)
  (:export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler interface

   ;; compiler
   #:clvba
   #:*clvba-stream*
   #:clvba-to-stream
   #:clvba-doc
   #:clvba-doc*
   #:clvba*

   ;; for clvba macro definition within lisp
   #:defclvbamacro
   #:defmacro+clvba
   #:*defined-operators*

   ;; gensym
   #:clvba-gensym
   #:with-clvba-gensyms
   #:*clvba-gensym-counter*

   ;; printer
   #:symbol-to-vba-string
   #:*vba-string-delimiter*
   #:*clvba-print-pretty*
   #:*indent-num-spaces*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language

   ;; literals
   #:t
   #:false
   #.(symbol-name 'nil) ; for case-sensitive Lisps like some versions of Allegro

   ;; array literals
   #:array
   #:aref

   ;; operators
   ;; logical boolean
   #:not
   #:and
   #:or
   #:*
   #:/
   #:+
   #:-
   #:negate
   #:mod
   #:<
   #:>
   #:<=
   #:>=
   #:equal
   #:eql
   #:eq
   #:=
   #:<>
   #:is


   ;; assignment
   #:|:=|
   
   ;; compile-time stuff
   #:eval-when

   ;; body forms
   #:progn

   ;; if
   #:if
   #:when
   #:unless

   ;; single argument statements
   #:return

   ;; assignment and binding
   #:setf
   #:psetf
   #:setq
   #:psetq
   #:let*
   #:let

   ;; variables
   #:dim
   #:var
   #:defvar
   #:type
   #:redim

   ;; iteration
   #:for
   #:for-in
   #:while
   ;; #:do
   ;; #:do*
   ;; #:dotimes
   ;; #:dolist
   ;; #:loop

   ;; case
   #:switch
   #:select-case
   #:default

   ;; function definition
   #:defun
   #:lambda

   ;; lambda lists
   #:&key
   #:&rest
   #:&body
   #:&optional
   #:&aux
   #:&environment
   #:&key-object

   ;; slot access
   #:getprop

   ;; macros
   #:macrolet
   #:symbol-macrolet
   #:define-symbol-macro
   #:define-clvba-symbol-macro
   #:defmacro

   ;; lisp eval
   #:lisp

   ;; utils
   #:1+
   #:1-
   #:concatenate
   #:stringify
   #:length
   #:@
   #:chain
   #:to

   ;; binding
   #:bind
   #:bind*

   ;; New Added
   #:with
   #:label
   #:public
   #:private
   #:raw
   #:subcall
   #:set
   #:defsub
   #:&
   #:on-error-goto
   #:on-error-resume-next
   #:on-error-goto-0
   #:exit

   #:clvbas
   #:addCC
   #:cell-split
   #:cell-text
   #:cell-align
   #:cell-vertical-align
   #:left-indent
   #:get-CC-tag
   #:get-CC-text
   #:set-cell-width
   #:set-border-bm
   #:create-table
   #:create-table*
   #:ae-bs-ddt
   #:row-text
   #:row-text01
   #:dm-freq
   #:dm-desc
   #:dm-freq-desc
   #:ss-freq
   #:column-header
   #:ss-column-header
   #:ss-column-header-subgroup
   #:cell-note
   #:cell-content-control
   #:create-label
   #:create-button
   #:create-combo-box
   #:create-text-box
   #:row-string
   #:combine-column-header
   #:new-legacy
   ))


(defpackage #:clvba-vba
  (:use)
  (:export
   ;; operators
   ;; arithmetic
   #:+
   #:-
   #:negate
   #:*
   #:/
   #:mod

   ;; assignment
   #:=
   #:|:=|

   ;; comparison
   #:<>
   #:>
   #:>=
   #:<
   #:<=
   #:is

   ;; logical
   #:and
   #:or
   #:not

   ;; misc
   #:|,|
   #:in

   ;; literals
   #:nil
   #:t
   #:false

   ;; statements
   #:block
   #:for
   #:for-in
   #:if
   #:label
   #:return
   #:select-case
   #:default
   #:dim
   #:set
   #:while
   #:with
   #:redim

   #:array
   #:aref
   #:cond
   #:lambda
   #:defun
   #:defsub
   #:getprop
   #:funcall
   #:subcall
   #:escape
   #:type

   ;; Other
   #:&
   #:raw
   #:to
   #:arg

   #:on-error-goto
   #:on-error-resume-next
   #:on-error-goto-0
   #:exit
   ))
