# lips

`Mini Lisp in (less than 200 lines of) Lisp` 

This is a very minimal Lisp interpreter written in Lisp for myself to study:

* interpreter basics
* basics of Lisp itself
* coding in Lisp (`SBCL`)
* project setup with Roswell
* writing tests (`fiveam`)

(...and of course it is not intended to be a full-fledged Lisp.)

#### Supported Features

* integers
* symbols with `define` (not ANSI CL)
* `if`
* lambda
* lexically-scoped variables
* `+`, `-`, `/`, `*` and `=`
* user-defined functions
* progn

###### Example Codes

Recursive fibonacci function:

```lisp
(define fibonacci (lambda (n) (if (= n 0) nil (if (= n 1) 0 (if (= n 2) 1 (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))))
(fibonacci 12) ; output: 89
```

#### How to Run

Clone the project under `local-projects` directory of Quicklisp or Roswell, and run commands as below.

###### Using Roswell

```sh
./roswell/lips.ros
```

###### Using Quicklisp

```lisp
(ql:quickload '(:lips) :silent t)
(lips:lips)
```

###### Exit
```lisp
(lips:exit)
```

#### Test
```lisp
(ql:quickload :fiveam)
(load #p"lips.asd")
(load #p"lips-test.asd")
(asdf:test-system :lips)
```


