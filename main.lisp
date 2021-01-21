;;; main.lisp
(in-package :cl-user)
(defpackage :lips
  (:use cl)
  (:export :lips))
(in-package :lips)

(defparameter *lips-error* nil)
(defstruct quote-obj expr)
(defstruct lambda-obj args codes)

(defun lips (&optional (stream-in nil) (stream-out nil) (one-line-exec nil))
    (setf *stream-in* (if stream-in stream-in *standard-input*))
    (setf *stream-out* (if stream-out stream-out *standard-output*))
  (lips-repl one-line-exec))

(defun lips-repl (&optional (one-line-exec nil))
  (write-string "> " *stream-out*)
  (finish-output)
  (let ((line (clean-line(read-line *stream-in* nil :eof))))
    (unless (equal line "(lips:exit)")
      (lips-print (lips-eval (lips-read (make-instance 'input-line :str line))))
      (unless one-line-exec
        (lips-repl)))))

;;; Parser

(defclass input-line ()
  ((str
    :initarg :str
    :accessor str)
   (head
    :initform 0
    :accessor head))
  (:documentation "Class for an input line with the character head."))

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
  
(defun lips-read (line)
  (when (has-more-str line)
    (let ((ch (peek-ch line)))
      (cond ((equal ch #\Space) (forward-head line) (lips-read line))
            ((equal ch #\() (forward-head line) (read-list line))
            ((equal ch #\') (forward-head line) (make-quote-obj :expr (lips-read line)))
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
  (if (equal #\) (peek-ch line))
    (progn (forward-head line)
           nil)
    (let ((token (lips-read line)))
      (if token
        (cons token (read-list line))
        nil))))

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
    (unless (member (peek-ch line) '(#\Space #\( #\)))
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
  (:documentation "Hash table for variable bindings with a pointer to the parent envionment."))

(defmethod search-bind ((obj env) key)
  (if (nth-value 1 (gethash key (bindings obj))) ; check the flag for key existance
    (gethash key (bindings obj))
    (if (parent obj)
      (search-bind (parent obj) key))))

(defparameter *global-env* (make-instance 'env))

; Built-in Functions
(setf (gethash "+" (bindings *global-env*)) (lambda (args) (apply #'+ args)))
(setf (gethash "-" (bindings *global-env*)) (lambda (args) (apply #'- args)))
(setf (gethash "*" (bindings *global-env*)) (lambda (args) (apply #'* args)))
(setf (gethash "/" (bindings *global-env*)) (lambda (args) (apply #'/ args)))
(setf (gethash "=" (bindings *global-env*)) (lambda (args) (apply #'= args)))

(defun lips-eval (form &optional (env *global-env*))
  (unless *lips-error*
    (typecase form
      (list
        (let ((f-name (car form))) ; Special Forms
          (cond ((equal "progn" f-name)
                 (car (last (mapcar (lambda (item) (lips-eval item env)) (cdr form)))))
                ((equal "if" f-name)
                  (if (lips-eval (cadr form) env)
                    (lips-eval (caddr form) env)
                    (lips-eval (cadddr form) env)))
                ((equal "define" f-name)
                 (let ((val (lips-eval (caddr form) env)))
                   (unless *lips-error*
                     (setf (gethash (cadr form) (bindings env)) val)
                     val)))
                ((equal "lambda" f-name)
                 (make-lambda-obj :args (cadr form) :codes (caddr form)))
                (t (let ((bind (search-bind env f-name)))
                     (typecase bind
                       (function
                         (funcall bind (mapcar (lambda (param) (lips-eval param env)) (cdr form))))
                       (quote-obj bind)
                       (lambda-obj
                         (let ((new-env (make-instance 'env :parent env)))
                           (mapcar (lambda (k v)
                                     (setf (gethash k (bindings new-env)) (lips-eval v env)))
                                   (lambda-obj-args bind)
                                   (cdr form))
                           (lips-eval (lambda-obj-codes bind) new-env)))
                       (t (setf *lips-error* "Illegal function call."))))))))
      (quote-obj form)
      (number form)
      (string (search-bind env form))
      (boolean form))))

;;; Output

(defun lips-print (res)
  (if *lips-error*
    (progn (write-line *lips-error* *stream-out*)
           (setf *lips-error* nil))
    (if (typep res 'quote-obj)
      (lips-print (quote-obj-expr res))
      (write-line 
        (typecase res
          (lambda-obj "lambda") 
          (integer (format nil "~D" res))
          (ratio (format nil "~F" res))
          (string res)
          (boolean (if res "T" "NIL")) ; (typep nil list) => t
          (list (format nil "(~{~A~^ ~})" res)))
         *stream-out*))))

;;; Generic

(defun list-to-string (lst)
    (format nil "~{~A~}" lst))

(defun num-str-length (val)
  (cond ((numberp val) (length (write-to-string val)))
        ((stringp val) (length val))))
