;;; main.lisp
(in-package :cl-user)
(defpackage :lips
  (:use cl)
  (:export :lips))
(in-package :lips)

(defvar *lips-error* nil)
(defvar *stream-in*)
(defvar *stream-out*)

;;; Main

(defun lips (&optional (stream-in nil) (stream-out nil) (one-line-exec nil))
    (setf *stream-in* (if stream-in stream-in *standard-input*))
    (setf *stream-out* (if stream-out stream-out *standard-output*))
  (lips-repl one-line-exec))

(defun lips-repl (&optional (one-line-exec nil))
  (write-string "> " *stream-out*)
  (finish-output)
  (let ((line (read-line *stream-in* nil :eof)))
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
 
(defstruct nil-obj)
(defstruct quote-obj expr)
(defstruct back-quote-obj expr)
(defstruct comma-obj expr)
(defstruct lambda-obj args codes)
(defstruct macro-obj params codes)
 
(defun lips-read (line &optional)
  (when (and (has-more-str line) (not *lips-error*))
    (let ((ch (peek-ch line)))
      (cond ((equal ch #\Space) (forward-head line) (lips-read line))
            ((equal ch #\() (forward-head line) (if (check-nil line)
                                                    (make-nil-obj)
                                                    (read-list line)))
            ((equal ch #\') (forward-head line) (make-quote-obj :expr (lips-read line)))
            ((equal ch #\`) (forward-head line) (make-back-quote-obj :expr (lips-read line)))
            ((equal ch #\,) (forward-head line) (make-comma-obj :expr (lips-read line))) 
            ((digit-char-p ch) (read-number line))
            (t (read-symbol line))))))

(defun check-nil (line)
  "Checks for an empty list."
  (if (equal #\) (peek-ch line))
    (progn (forward-head line)
           t)))

(defun read-list (line)
  (let ((ch (peek-ch line)))
    (cond
      ((equal ch #\Space)
       (forward-head line) (read-list line))
      ((equal ch #\))
       (forward-head line) nil)
      (t (let ((token (lips-read line)))
           (if token
               (cons token (read-list line))
               nil))))))
       
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
  "Searches for a bind by name towards parent scopes. Sets error value if not found."
  (if (nth-value 1 (gethash key (bindings obj))) ; check the flag for key existance
    (gethash key (bindings obj))
    (if (parent obj)
      (search-bind (parent obj) key)
      (setf *lips-error* (format nil "~S is unbound." key)))))

(defmethod add-bind ((obj env) sym val)
  (setf (gethash sym (bindings obj)) val))

(defparameter *global-env* (make-instance 'env))

; NIL Symbol
(setf (gethash "nil" (bindings *global-env*)) nil)

; Built-in Functions
(setf (gethash "+" (bindings *global-env*)) (lambda (args) (apply #'+ args)))
(setf (gethash "-" (bindings *global-env*)) (lambda (args) (apply #'- args)))
(setf (gethash "*" (bindings *global-env*)) (lambda (args) (apply #'* args)))
(setf (gethash "/" (bindings *global-env*)) (lambda (args) (apply #'/ args)))
(setf (gethash "=" (bindings *global-env*)) (lambda (args) (apply #'= args)))

; Special Forms
(defun s-form-progn (form env)
  (car (last (mapcar (lambda (item) (lips-eval item env))
                     form))))

(defun s-form-if (form env)
  (if (lips-eval (car form) env)
    (lips-eval (cadr form) env)
    (lips-eval (caddr form) env)))

(defun s-form-define (form env)
  (let ((val (lips-eval (cadr form) env)))
    (unless *lips-error*
      (add-bind env (car form) val)
      val)))

(defun s-form-defmacro (form env)
  "Macro can be defined by backquote, quote or direct expression."
  (typecase (caddr form)
    (quote-obj (add-bind env (car form) (make-macro-obj :codes (caddr form))))
    (back-quote-obj (add-bind env (car form) (make-macro-obj :params (cadr form) :codes (caddr form))))
    (t (let ((val (lips-eval (caddr form) env)))
         (unless *lips-error*
           (add-bind env (car form) (make-macro-obj :params (cadr form) :codes val)))))))

(defun s-form-macroexpand (form env)
  (let* ((quote-expr (quote-obj-expr (lips-eval (car form) env)))
         (bind (search-bind env (car quote-expr))))
    (expand-macro (macro-obj-codes bind) (macro-obj-params bind) (cdr quote-expr) env)))

(defun call-func (func arg-forms env)
  (funcall func (mapcar (lambda (param) (lips-eval param env))
                        arg-forms)))
 
(defun call-lambda (obj arg-value-forms env)
  (lips-eval (lambda-obj-codes obj)
             (create-lexical-env (lambda-obj-args obj) arg-value-forms env t)))

(defun call-macro (obj arg-value-forms env)
  (let ((codes (macro-obj-codes obj))
        (param-names (macro-obj-params obj)))
    (typecase codes
      (back-quote-obj
        (lips-eval (quote-obj-expr
                     (expand-macro codes param-names arg-value-forms env))
                   env))
      (quote-obj (lips-eval (quote-obj-expr codes) env))
      (t (lips-eval codes (create-lexical-env param-names arg-value-forms env t))))))

(defun expand-macro (codes param-names arg-value-forms env)
  (let ((new-env (create-lexical-env param-names arg-value-forms env nil)))
    (typecase codes
      (back-quote-obj (interpolate-back-quote codes new-env nil))
      (quote-obj (quote-obj-expr codes))
      (t codes))))

(defun create-lexical-env (arg-names arg-value-forms env eval-val)
  "Creates the bindings for function parameters lexically. Also used for
the back quote interpolation of macro without evaluating the values."
  (let ((new-env (make-instance 'env :parent env)))
    (mapcar (lambda (k v)
              (add-bind new-env k 
                (if eval-val 
                  (lips-eval v env)
                  v)))
            arg-names
            arg-value-forms)
    new-env))

(defun interpolate-back-quote (obj env eval-comma)
  "Interpolates the comma symbols with a given env. For macros this
function is used without evaluating the comma symbols."
  (let ((expr (back-quote-obj-expr obj)))
    (make-quote-obj
      :expr (typecase expr
              (list (mapcar
                      (lambda (item)
                        (if (typep item 'comma-obj)
                            (if eval-comma
                                (lips-eval (comma-obj-expr item) env)
                                (search-bind env (comma-obj-expr item)))
                            item))
                      expr))
              (comma-obj (if eval-comma
                             (lips-eval (comma-obj-expr expr) env)
                             (search-bind env (comma-obj-expr expr))))
              (t expr)))))

(defun lips-eval (form &optional (env *global-env*))
  (unless *lips-error*
    (typecase form
      (list
        (let ((f-name (car form)))
          (cond ((equal "progn" f-name) (s-form-progn (cdr form) env))
                ((equal "if" f-name) (s-form-if (cdr form) env))
                ((equal "define" f-name) (s-form-define (cdr form) env))
                ((equal "lambda" f-name) (make-lambda-obj :args (cadr form) :codes (caddr form)))
                ((equal "defmacro" f-name) (s-form-defmacro (cdr form) env))
                ((equal "macroexpand" f-name) (s-form-macroexpand (cdr form) env))
                (t (let ((bind (search-bind env f-name)))
                     (typecase bind
                       (quote-obj bind)
                       (function (call-func bind (cdr form) env))
                       (lambda-obj (call-lambda bind (cdr form) env))
                       (macro-obj (call-macro bind (cdr form) env))
                       (t (setf *lips-error* "Illegal function call."))))))))
      (nil-obj nil)
      (quote-obj form)
      (back-quote-obj (interpolate-back-quote form env t))
      (number form)
      (string (search-bind env form))
      (boolean form))))

;;; Output

(defmethod print-object ((obj nil-obj) out)
    (format out "NIL"))

(defun lips-print (res)
  (if *lips-error*
    (progn (write-line *lips-error* *stream-out*)
           (setf *lips-error* nil))
    (if (typep res 'quote-obj)
      (lips-print (quote-obj-expr res))
      (write-line 
        (typecase res
          (lambda-obj "lambda") 
          (macro-obj "macro")
          (integer (format nil "~D" res))
          (ratio (format nil "~F" res))
          (string res)
          (boolean (if res "T" "NIL")) ; (typep nil list) => t
          (list (format nil "(~{~A~^ ~})" res))
          (t "Unsupported type."))
         *stream-out*))))

;;; Generic

(defun list-to-string (lst)
    (format nil "~{~A~}" lst))

(defun num-str-length (val)
  (cond ((numberp val) (length (write-to-string val)))
        ((stringp val) (length val))))
