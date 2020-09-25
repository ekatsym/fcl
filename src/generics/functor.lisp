(defpackage fcl.generics.functor
  (:nicknames :fcl.g.functor :fcl.functor)
  (:use :common-lisp)
  (:export
    #:fmap))
(in-package :fcl.generics.functor)


(defgeneric fmap (a->b a*)
  (:documentation
"Returns a value of class of B* including (FUNCALL A->B A)
where A is a value included in A*.
FMAP must satisfy the rules:
  Identity:    (fmap #'identity a*)
            == (identity a*)
  Composition: (fmap (lambda (a) (b->c (a->b a))) a*)
            == (fmap #'b->c (fmap #'a->b a*))"))
