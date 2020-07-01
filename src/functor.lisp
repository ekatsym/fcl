(defpackage fcl.functor
  (:use :common-lisp)
  (:import-from
    :fcl
    #:fmap)
  (:export
    #:fmap))
(in-package :fcl.functor)


(defgeneric fmap (a->b a*)
  (:documentation
"Returns a value of class of B* including (FUNCALL A->B A)
where A is a value included in A*.
FMAP must satisfy the rules:
  Identity:    (fmap #'identity a*)
            == (identity a*)
  Composition: (fmap (lambda (a) (b->c (a->b a))) a*)
            == (fmap #'b->c (fmap #'a->b a*))"))
