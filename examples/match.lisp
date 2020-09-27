(require :fcl)
(defpackage fcl-user
  (:use :common-lisp
        :fcl.lazy
        :fcl.data
        :fcl.match))
(in-package :fcl-user)

(match '()
  (nil        (print (list "matched with NIL pattern" nil)))
  ((cons a b) (print (list "matched with CONS pattern" a b))))

(match (list 0 1 2)
  (nil        (print (list "matched with NIL pattern" nil)))
  ((cons a b) (print (list "matched with CONS pattern" a b))))

(match (list 0 1 2 3)
  ((list a)       (print (list "matched with 1st pattern" a)))
  ((list a 1)     (print (list "matched with 2nd pattern" (+ a 1))))
  ((list a 0 c)   (print (list "matched with 3rd pattern" (+ a 0 c))))
  ((list a b 2 2) (print (list "matched with 4rd pattern" (+ a b 2 2))))
  ((list a b c 3) (print (list "matched with 5rd pattern" (+ a b c 3)))))

(match (list 0 (list 1 (list 2 3) 4) 5)
  ((list a b c d e f)               (print (list "matched with flat pattern" (+ a b c d e f))))
  ((list a (list b (list c d) e) f) (print (list "matched with nested pattern" (+ a b c d e f)))))

(match (vector 0 1 2)
  ((list 0 b c)   (print (list "matched with LIST pattern" (+ b c))))
  ((vector 0 b c) (print (list "matched with VECTOR pattern" (* b c)))))

(match "world"
  ("hello" (print (list "matched with \"hello\"" "hello")))
  ("world" (print (list "matched with \"world\"" "world"))))

(match (delay (* 3 5))
  ((delay x) (print (list "matched with LAZY pattern" x))))

(defdata point-data (point1 t t))
(defstruct point2 x y)
(match (make-point2 :x 3 :y 7)
  ((point1 x y)       (print (list "matched with ALGEBRAIC-DATATYPE pattern" (+ x y))))
  ((point2 :x x :y y) (print (list "matched with normal CLASS pattern" (* x y)))))

(defdata maybe
  (nothing)
  (just t))
(match (nothing)
  ((nothing) (print (list "mathced with NOTHING pattern" 0)))
  ((just x)  (print (list "mathced with JUST pattern" x))))
(match (just 10)
  ((nothing) (print (list "mathced with NOTHING pattern" 0)))
  ((just x)  (print (list "mathced with JUST pattern" x))))
nil
