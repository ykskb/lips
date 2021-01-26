(in-package :cl-user)
(defpackage lips-test
  (:use :cl :fiveam :lips))
(in-package lips-test)

(def-suite :lips)
(in-suite :lips)

(defun run-lips (input)
  (let ((output (make-string-output-stream)))
    (lips:lips (make-string-input-stream input) output t)
    (string-trim '(#\Space #\Tab #\Newline)
                 (subseq (get-output-stream-string output) 2)))) 

(test lips-primitive
  (is (string= "1" (run-lips "1")))
  (is (string= "(1 2)" (run-lips " ' ( 1 2 ) ")))
  (is (string= "NIL" (run-lips "()")))
  (is (string= "NIL" (run-lips "nil"))))

(test lips-illegal-func-calls
  (is (string= "Illegal function call." (run-lips "(1)")))
  (is (string= "Illegal function call." (run-lips "(a)")))
  (is (string= "Illegal function call." (run-lips "(3 a)")))
  (is (string= "Illegal function call." (run-lips "(progn (define fn (lambda (n) (n))) (fn 3))"))))

(test lips-built-in-funcs
  (is (string= "6" (run-lips "(+ 1 2 3)")))
  (is (string= "1" (run-lips "(- 5 3 1)")))
  (is (string= "8" (run-lips "(* 2 2 2)")))
  (is (string= "1" (run-lips "(/ 9 3 3)")))
  (is (string= "1.5" (run-lips "(/ 3 2)")))
  (is (string= "NIL" (run-lips "(= 1 1 3)")))
  (is (string= "T" (run-lips "(= 1 1 1)"))))

(test lips-quote
  (is (string= "a" (run-lips "'a")))
  (is (string= "1" (run-lips "'1")))
  (is (string= "(1 2)" (run-lips "'(1 2)"))))

(test lips-progn
  (is (string= "2" (run-lips "(progn 1 2)")))
  (is (string= "2" (run-lips "(progn (+ 1 2) 2)")))
  (is (string= "2" (run-lips "(progn 1 (+ 1 1))"))))

(test lips-lambda
  (is (string= "lambda" (run-lips "(lambda (n) (+ n 5))")))
  (is (string= "8" (run-lips "(progn (define fn (lambda (n) (+ n 5))) (fn 3))"))))

(test lips-define
  (is (string= "5" (run-lips "(define n 5)")))
  (is (string= "lambda" (run-lips "(define fn (lambda (n) (+ n 3)))")))
  (is (string= "5" (run-lips "(progn (define n 5) n)"))))

(test lips-back-quote
  (is (string= "5" (run-lips "`5")))
  (is (string= "a" (run-lips "`a")))
  (is (string= "(a b)" (run-lips "`'(a b))")))
  (is (string= "5" (run-lips "(progn (define n 5) `,n)"))))

(test lips-macro
  (is (string= "macro" (run-lips "(defmacro mac (n) `(+ 3 ,n))")))
  (is (string= "9" (run-lips "(progn (defmacro mac (n) `(+ 3 ,n)) (mac 6))")))
  (is (string= "(+ 3 6)" (run-lips "(progn (defmacro mac (n) `(+ 3 ,n)) (macroexpand '(mac 6)))")))
  (is (string= "(+ 1 (- 3 1))" (run-lips "(progn (defmacro mac (n) `(+ 1 ,n)) (macroexpand '(mac (- 3 1))))")))
  (is (string= "3" (run-lips "(progn (defmacro mac (n) `(+ 1 ,n)) (mac (- 3 1)))")))
  )

