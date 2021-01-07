;;; main.lisp
(in-package :cl-user)
(defpackage :misp
  (:use cl)
  (:export :misp))
(in-package :misp)

(defun misp (input-stream output-stream)
  (let ((line (read-line input-stream nil :eof)))
    (write-line line output-stream)))
