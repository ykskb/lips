;;; misp.asd
(in-package :cl-user)
(defpackage :misp-asd
  (:use :cl :asdf))
(in-package :misp-asd)

(defsystem :misp
  :class :package-inferred-system
  :description "Mini Lisp in Lisp"
  :version "0.1"
  :author "yohei"
  :license "MIT"
  :depends-on ("misp/main")
  :in-order-to ((test-op (test-op misp-test))))
