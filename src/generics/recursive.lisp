(defpackage fcl.generics.recursive
  (:nicknames :fcl.g.recursive :fcl.recursive)
  (:use :common-lisp)
  (:export
    #:fmap
    #:cata
    #:para
    #:ana
    #:apo))
(in-package :fcl.generics.recursive)


(defgeneric cata (x*->x i))

(defgeneric para (i&a*->a i))

(defgeneric ana (x->x* x))

(defgeneric apo (x->f+x* x))
