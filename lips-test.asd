;;; lips-test.asd
(in-package :cl-user)
(defpackage :lips-test-asd
  (:use :cl :asdf :uiop))
(in-package :lips-test-asd)

(defsystem :lips-test
  :class :package-inferred-system
  :description "Tests for Lips"
  :version "0.1"
  :author "yohei"
  :license "MIT"
  :depends-on (:lips :fiveam)
  :components ((:module "tests" :components ((:file "main"))))
  :perform (test-op (o s)
            (uiop:symbol-call :fiveam :run! :lips)))
