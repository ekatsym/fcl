(defpackage fcl.monoid
  (:use :common-lisp)
  (:import-from
    :fcl
    #:mzero
    #:mplus
    #:msum)
  (:import-from
    :fcl.monad
    #:unit)
  (:export
    #:mzero
    #:mplus
    #:msum))
(in-package :fcl.monoid)


(defgeneric mzero (class)
  (:documentation
"Returns identity of CLASS for MPLUS."))

(defgeneric mplus (monoid1 monoid2)
  (:documentation
"Returns \"sum\" of MONOID1 and MONOID2.
NIL is identity of class T for MPLUS.
MPLUS must satisfy the rules:
  Identity:      (mplus (mzero) m)
              == (mplus m (mzero))
              == m
  Associativity: (mplus (mplus m1 m2) m3)
              == (mplus m1 (mplus m2 m3))"))

(defmethod mplus ((monoid1 null) monoid2)
  monoid2)

(defmethod mplus (monoid1 (monoid2 null))
  monoid1)

(defun msum (&rest monoids)
  (reduce #'mplus monoids
          :from-end t))
