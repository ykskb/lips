# misp

`Mini Lisp in (less than 200 lines of) Lisp` 

This is a very minimal Lisp interpreter written in  Lisp for myself to study:

* interpreter basics
* basics of Lisp itself
* coding in Lisp (SBCL)
* project setup with Roswell
* writing tests

and it is not intended to be full-fledged Lisp.

##### Supported Features

* integers
* symbols with `define` (not ANSI)
* `if`
* lambda
* lexically-scoped variables
* `+`, `-`, `/`, `*` and `=`
* user-defined functions

##### Example Codes

Recursive fibonacci function:

```lisp
(define fibonacci (lambda (n) (if (= n 0) nil (if (= n 1) 0 (if (= n 2) 1 (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))))
(fibonacci 12) ; output: 89
```

##### How to Run

Clone the project under `local-projects` directory of Quicklisp or Roswell and run commands as below.

###### Using Roswell

```sh
./roswell/misp.ros
```

###### Using Quicklisp

```lisp
(ql:quickload '(:misp) :silent t)
(misp:misp)
```


