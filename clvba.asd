;;;; -*- lisp -*-

(defsystem :clvba
  :name "CLVBA"
  :author "Kenneth Yan <yanxiaoguang@gmail.com>"
  :maintainer "Kenneth Yan <yanxiaoguang@gmail.com>"
  :licence "BSD"
  :serial t
  :components
  ((:static-file "clvba.asd")
   (:file "package")
   (:file "utils")
   (:file "compiler")
   (:file "printer")
   (:file "operator")
   (:file "function-definition")
   (:file "macros"))
  :depends-on (:cl-ppcre :anaphora :named-readtables :port))
