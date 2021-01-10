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
  (write-string "> " *stream-out*)
  (finish-output)
  (let ((line (clean-line(read-line *stream-in* nil :eof))))
    (unless (equal line "(misp:exit)")
      (misp-print (misp-eval (misp-read line)))
      (misp-repl))))

;;; Parser

(defun misp-read (line &optional (pos 0))
  (when (< pos (length line))
    (let ((ch (char line pos)))
      (cond ((equal ch #\Space) (misp-read line (1+ pos)))
            ((equal ch #\() (read-list (subseq line (1+ pos))))
            ((digit-char-p ch) (read-number (subseq line pos)))
            (t (read-symbol (subseq line pos)))))))

(defun clean-line (line)
  (list-to-string 
    (remove-extra-spaces 
      (string-trim '(#\Space #\Tab #\Newline) line) 0 nil)))

(defun remove-extra-spaces (line pos spaced)
  (if (< pos (length line))
    (let ((ch (char line pos)))
      (if (equal ch #\Space)
        (if spaced
          (remove-extra-spaces line (1+ pos) t)
          (cons ch (remove-extra-spaces line (1+ pos) t)))
        (cons ch (remove-extra-spaces line (1+ pos) nil))))
    nil))

(defun read-list (line &optional (pos 0))
  (when (and (< pos (length line)) 
        (not (equal #\) (char line pos))))
    (let ((token (misp-read (subseq line pos))))
      (if token
        (cons token (read-list line (1+ (+ pos (num-str-length token)))))
        nil))))

(defun read-number (line)
  (parse-integer (list-to-string (read-number-chars line))))
 
(defun read-number-chars (line &optional (pos 0))
  (if  (< pos (length line))
    (let ((ch (char line pos)))
      (when (digit-char-p ch)
        (cons ch (read-number-chars line (1+ pos)))))
    nil))

(defun read-symbol (line &optional (pos 0))
  (list-to-string (read-symbol-chars line)))

(defun read-symbol-chars (line &optional (pos 0))
  (if (< pos (length line))
    (let ((ch (char line pos)))
      (unless (member ch '(#\space #\( #\)))
        (cons ch (read-symbol-chars line (1+ pos)))))
    nil))

;;; Evaluator

(defun misp-eval (token)
  (cond ((listp token) token)
        ((numberp token) token)
        ((stringp token) token)))

;;; Output

(defun misp-print (res)
  (write-line 
    (cond ((integerp res) (format nil "~D" res))
          ((stringp res) res)
          ((listp res) (format nil "(~{~A ~})" res)))
     *stream-out*))

;;; Generic

(defun list-to-string (lst)
    (format nil "~{~A~}" lst))

(defun num-str-length (val)
  (cond ((numberp val) (length (write-to-string val)))
        ((stringp val) (length val))))
