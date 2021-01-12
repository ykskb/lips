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
      (misp-print (misp-eval (misp-read (make-instance 'input-line :str line))))
      (misp-repl))))

;;; Parser

(defclass input-line (); Tried functional approach, but mutual recursion for reading list ended up ugly...
  ((str
    :initarg :str
    :accessor str)
   (head
    :initform 0
    :accessor head))
  (:documentation "Input line class with the character head."))

(defmethod has-more-str ((obj input-line))
  (< (head obj) (length (str obj))))

(defmethod forward-head ((obj input-line))
  (setf (head obj) (1+ (head obj)))) 

(defmethod peek-ch ((obj input-line))
  (char (str obj) (head obj)))

(defmethod read-ch ((obj input-line))
  (let ((ch (peek-ch obj)))
    (forward-head obj)
    ch))
  
(defun misp-read (line)
  (when (has-more-str line)
    (let ((ch (peek-ch line)))
      (cond ((equal ch #\Space) (forward-head line) (misp-read line))
            ((equal ch #\() (forward-head line) (read-list line))
            ((digit-char-p ch) (read-number line))
            (t (read-symbol line))))))

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

(defun read-list (line)
  (if (and (has-more-str line) 
           (not (equal #\) (peek-ch line))))
    (let ((token (misp-read line)))
      (if token
        (cons token (read-list line))
        nil))
    nil))

(defun read-number (line)
  (parse-integer (list-to-string (read-number-chars line))))
 
(defun read-number-chars (line)
  (if (has-more-str line)
    (when (digit-char-p (peek-ch line))
      (cons (read-ch line) (read-number-chars line)))
    nil))

(defun read-symbol (line)
  (list-to-string (read-symbol-chars line)))

(defun read-symbol-chars (line)
  (if (has-more-str line)
    (unless (member (peek-ch line) '(#\space #\( #\)))
      (cons (read-ch line) (read-symbol-chars line)))
    nil))

;;; Evaluator

 (defclass env ()
  ((parent
    :initarg :parent
    :initform nil
    :accessor parent)
   (bindings
    :initform (make-hash-table :test 'equal)
    :accessor bindings))
  (:documentation "Binding hash map with a pointe to the parent envionment."))

(defmethod search-bind ((obj env) key)
  (if (nth-value 1 (gethash key (bindings obj)))
    (gethash key (bindings obj))
    (if (parent obj)
      (search-bind (parent obj) (key))
      (format *stream-out* "~S is undefined." key))))

(defparameter *global-env* (make-instance 'env))

(defun misp-eval (form &optional (env *global-env*))
  (cond ((listp form) 
          (let ((f-name (car form)))
            (cond ((equal "if" f-name)
                   (if (misp-eval (cadr form))
                     (misp-eval (caddr form))
                     (misp-eval (cadddr form))))
                  ((equal "def" f-name)
                   (setf (gethash (cadr form) (bindings env)) (caddr form))
                   (caddr form))) ))
        ((numberp form) form)
        ((stringp form) (search-bind env form))))

;;; Output

(defun misp-print (res)
  (write-line 
    (cond ((integerp res) (format nil "~D" res))
          ((stringp res) res)
          ((listp res) (format nil "(~{~A~^ ~})" res)))
     *stream-out*))

;;; Generic

(defun list-to-string (lst)
    (format nil "~{~A~}" lst))

(defun num-str-length (val)
  (cond ((numberp val) (length (write-to-string val)))
        ((stringp val) (length val))))

;;; Util

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
                using (hash-value value)
                collect (list key value))))

