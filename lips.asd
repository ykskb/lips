;;; lips.asd
(in-package :cl-user)
(defpackage :lips-asd
  (:use :cl :asdf))
(in-package :lips-asd)

(defsystem :lips
  :class :package-inferred-system
  :description "Mini Lisp in Lisp"
  :version "0.1"
  :author "yohei"
  :license "MIT"
  :depends-on ("lips/main")
  :in-order-to ((test-op (test-op lips-test))))
