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
;-> FIB

(let (($x (time (delay (fib 40))))) ; (fib 40) is delayed and its promise is binded to $x
  (time (force $x))                 ; the evaluation of (fib 40) is forced
  (time (force $x)))                ; the cached value is returned
;-> 165580141
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

The algebraic data type is a kind of composite type and consisting of direct
sum, direct product, and recursion. It is often used with pattern matching in
functional programming. In common lisp, a new composite type is defined as
structure or class. In FCL, algebraic data type is named `algebraic-datatype`
and implemented by structure and can be defined using `defdata`. In addition,
`defdata` can define lazy constructors.

```lisp
(defdata maybe
  (nothing)
  (just t))
;-> MAYBE

(nothing)
;-> #.(NOTHING)

(just 10)
;-> #.(JUST 10)

(defdata lazy-stream
  (lazy-nil)
  (lazy-cons (:lazy t) lazy-stream))
;-> LAZY-STREAM

(lazy-nil)
;-> #.(LAZY-NIL)

(lazy-cons 0 (lazy-cons 1 (lazy-nil)))
;-> #.(LAZY-CONS #<PROMISE 0> #.(LAZY-CONS #<PROMISE 1> #.(LAZY-NIL)))
```

### Pattern Matching
In FCL, pattern matching is provided by `match` and `ematch` macros. `ematch` is
a version of `match` that an error is signalled when any patterns do not match
the given datum. The valid match symtax is defined as follows:

```text
match datum clause* => result*

clause ::= (pattern form*)
pattern ::= cons-pattern |
            t |
            nil |
            _ |
            variable |
            literal
cons-pattern ::= (quote pattern) |
                 (cons pattern pattern) |
                 (list pattern*) |
                 (vector pattern*) |
                 (delay pattern) |
                 (adata-name pattern*) |
                 (class-name {slot-name pattern}*)

datum --- an object.
forms --- an implicit progn.
variable --- a symbol except t, nil or _.
literal --- an atom except symbol.
adata-name --- a algebraic-datatype name.
class-name --- a class name except algebraic-datatype.
slot-name --- a slot name.
results --- the values returned by forms in matching clause.
```

### Generic Functions in Special Packages

#### Example: Maybe Monad

#### Example: List Monad, List Monoid and List Comprehension

#### Example: State Monad

## Installation

## Author

* ekatsym

## Copyright

Copyright (c) 2020 ekatsym
