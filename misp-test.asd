;;; misp-test.asd
(in-package :cl-user)
(defpackage :misp-test-asd
  (:use :cl :asdf :uiop))
(in-package :misp-test-asd)

(defsystem :misp-test
  :class :package-inferred-system
  :description "Tests for Misp"
  :version "0.1"
  :author "yohei"
  :license "MIT"
  :depends-on (:misp :fiveam)
  :components ((:module "tests" :components ((:file "main"))))
  :perform (test-op (o s)
            (uiop:symbol-call :fiveam :run! :misp)))
