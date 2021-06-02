(defpackage fcl.functor
  (:nicknames :fcl.generics.functor :fcl.ft)
  (:use :common-lisp)
  (:export #:fmap))
(in-package :fcl.functor)


(defgeneric fmap (a->b a*)
  (:documentation
"Returns a value of class B* including (FUNCALL A->B A)
where A is a value included in A*.
FMAP must satisfy the rules:
  Identity:    (fmap #'identity a*)
            == (identity a*)
  Composition: (fmap (lambda (a) (b->c (a->b a))) a*)
            == (fmap #'b->c (fmap #'a->b a*))"))
