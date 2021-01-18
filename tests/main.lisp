(in-package :cl-user)
(defpackage misp-test
  (:use :cl :fiveam :misp))
(in-package misp-test)

(def-suite :misp)
(in-suite :misp)

(defun run-misp (input)
  (let ((output (make-string-output-stream)))
    (misp:misp (make-string-input-stream input) output t)
    (string-trim '(#\Space #\Tab #\Newline)
                 (subseq (get-output-stream-string output) 2)))) 

(test misp-primitive
  (is (string= "1" (run-misp "1"))))

(test misp-built-in-funcs
  (is (string= "6" (run-misp "(+ 1 2 3)")))
  (is (string= "1" (run-misp "(- 5 3 1)")))
  (is (string= "8" (run-misp "(* 2 2 2)")))
  (is (string= "1" (run-misp "(/ 9 3 3)")))
  (is (string= "1.5" (run-misp "(/ 3 2)")))
  (is (string= "NIL" (run-misp "(= 1 1 3)")))
  (is (string= "T" (run-misp "(= 1 1 1)"))))

(test misp-progn
  (is (string= "2" (run-misp "(progn 1 2)")))
  (is (string= "2" (run-misp "(progn (+ 1 2) 2)")))
  (is (string= "2" (run-misp "(progn 1 (+ 1 1))"))))

(test misp-lambda
  (is (string= "lambda" (run-misp "(lambda (n) (+ n 5))")))
  (is (string= "8" (run-misp "(progn (define fn (lambda (n) (+ n 5))) (fn 3))"))))

(test misp-define
  (is (string= "5" (run-misp "(define n 5)")))
  (is (string= "lambda" (run-misp "(define fn (lambda (n) (+ n 3)))")))
  (is (string= "5" (run-misp "(progn (define n 5) n)"))))


