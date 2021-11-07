(defpackage fcl.recursive
  (:nicknames :fcl.generics.recursive :fcl.rc)
  (:use :common-lisp)
  (:import-from :fcl.adata #:defdata)
  (:import-from :fcl.match #:ematch)
  (:import-from :fcl.util #:partial #:group)
  (:export
    #:polynomial #:term
    #:cata #:para #:ana #:apo))
(in-package :fcl.generics.recursive)


;;; Polynomial
(defdata polynomial
  (term symbol list))

;;; Catamorphisms
(defgeneric cata (ax->x a*)
  (:documentation
"Catamorphism CATA destructures A* to polynomial AX of X on A, applies AX to AX->X and returns the result X."))

(defgeneric para (ax&a*->x a*)
  (:documentation
"Paramorphism PARA destructures A* to polynomial AX of X on A, applies AX and A*, to AX&A*->X and returns the result X."))

(defgeneric ana (class x->ax x)
  (:documentation
"Anamorphism ANA applies X to X->AX and constructs a value of algebraic data type on A from the result polynomial AX of X on A."))

(defgeneric apo (class x->ax+a0 x)
  (:documentation
"Apomorphism APO appliec X to X->AX+A0 and constructs a value of algebraic data type on A from the result polynomial AX of X on A or A0."))
