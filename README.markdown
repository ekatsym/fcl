# FCL
FCL is a library for functional programming in Common Lisp.
FCL does not depend on any other systems.

## Usage

### Lazy Evaluation

The `delay` macro delays evaluation of given expression and creates an object
of `promise` class. The `force` function forces the delayed evaluation in the
`promise` object and returns returns the value; since `promise` has a cache
facility, when a `promise` object that has already been evaluated is applied to
`force`, the evaluation is not forced and the cached value is returned.

```lisp
(defun fib (n)
  (case n
    ((0 1)     1)
    (otherwise (+ (fib (- n 1)) (fib (- n 2))))))

(let (($x (time (delay (fib 40))))) ; (fib 40) is delayed and its promise is binded to $x
  (time (force $x))                 ; the evaluation of (fib 40) is forced
  (time (force $x)))                ; the cached value is returned

;=> Evaluation took:
;=>   0.000 seconds of real time
;=>   0.000001 seconds of total run time (0.000001 user, 0.000000 system)
;=>   100.00% CPU
;=>   704 processor cycles
;=>   0 bytes consed
;=>
;=> Evaluation took:
;=>   1.380 seconds of real time
;=>   1.361020 seconds of total run time (1.360587 user, 0.000433 system)
;=>   98.62% CPU
;=>   4,702,054,256 processor cycles
;=>   0 bytes consed
;=>
;=> Evaluation took:
;=>   0.000 seconds of real time
;=>   0.000001 seconds of total run time (0.000001 user, 0.000000 system)
;=>   100.00% CPU
;=>   1,766 processor cycles
;=>   0 bytes consed
```

### Algebraic Data Type

### Pattern Matching

### Generic Functions in Special Packages

#### Example: Maybe Monad

#### Example: List Monad, List Monoid and List Comprehension

#### Example: State Monad

## Installation

## Author

* ekatsym

## Copyright

Copyright (c) 2020 ekatsym
