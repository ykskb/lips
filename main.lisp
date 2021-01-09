;;; main.lisp
(in-package :cl-user)
(defpackage :misp
  (:use cl)
  (:export :misp))
(in-package :misp)

(defparameter *stream-in* *standard-input*)
(defparameter *stream-out* *standard-output*)

(defun misp (&optional (stream-in nil) (stream-out nil))
  (when stream-in
    (setf *stream-in* stream-in))
  (when stream-out
    (setf *stream-out* stream-out))
  (misp-repl))
  
(defun misp-repl ()
  (let ((line (read-line *stream-in* nil :eof)))
    (unless (equal line "misp:quit")
      (write-line (misp-read line) *stream-out*)
      (misp-repl))))

(defun misp-read (line &optional (pos 0))
  (when (< pos (length line))
    (let ((ch (char line pos)))
      (cond ((equal ch #\Space) (misp-read line (1+ pos)))
            ((digit-char-p ch) (list-to-string (read-number-chars (subseq line pos))))
            (t "not a number")))))
  
(defun list-to-string (lst)
    (format nil "~{~A~}" lst))

(defun read-number-chars (line &optional (pos 0))
  (if  (< pos (length line))
    (let ((ch (char line pos)))
      (when (digit-char-p ch)
        (cons ch (read-number-chars line (1+ pos)))))
    nil))
