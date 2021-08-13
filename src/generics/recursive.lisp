(defpackage fcl.recursive
  (:nicknames :fcl.generics.recursive :fcl.rc)
  (:use :common-lisp :fcl.functor)
  (:export
    #:cata #:para #:ana #:apo

    ;;; Functor
    #:fmap))
(in-package :fcl.generics.recursive)


(defgeneric cata (x*->x i))

(defgeneric para (i&*x->x i))

(defgeneric ana (class x->x* x))

(defgeneric apo (class x->f+*x x))
