# lips

`Mini Lisp in (less than 300 lines of) Lisp` 

This is a very minimal Lisp interpreter written in Lisp. I made it since I wanted to learn Lisp as well as how to set up a Lisp project with Roswell and a testing library. It's nowhere near a full-fledged Lisp, nor does it intend to be, yet I found it quite fun to code Lisp to make Lisp :)

#### Supported Features

* `nil`
* integers
* symbols with `define` (not ANSI CL)
* `if`
* lambda
* `+`, `-`, `/`, `*` and `=`
* user-defined functions
* lexically-scoped variables
* progn
* quote (data)
* back quote interpolation
* macro
* macroexpand

###### Examples of Supported Codes

* Recursive fibonacci function

```lisp
(define fibonacci (lambda (n) (if (= n 0) nil (if (= n 1) 0 (if (= n 2) 1 (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))))
(fibonacci 12)
; 89
```

* `unless` macro

```lisp
(defmacro unless (cond body) `(if ,cond () ,body))
(macroexpand '(unless (= 3 4) 1))
; (if (= 3 4) NIL 1)
(unless (= 3 4) 1)
; 1
```

* Back quote interpolation & evaluation

```lisp
(define n 3)
`,(+ 2 n)
; 5
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


