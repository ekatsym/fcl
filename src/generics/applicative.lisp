(defpackage fcl.generics.applicative
  (:nicknames :fcl.g.applicative :fcl.applicative)
  (:use
    :common-lisp
    :fcl.generics.functor)
  (:export
    #:fmap
    #:unit
    #:amap
    #:define-fmap-by-applicative))
(in-package :fcl.generics.applicative)


(defgeneric unit (class a)
  (:documentation
"Returns a minimal unit value of CLASS including A."))

(defgeneric amap (a->*b a*)
  (:documentation
"Returns a value of class of B* including (FUNCALL A->B A)
where A->B and A are values included A->*B and A*.
AMAP must satisfy the rules:
  Identity:     (amap (unit class #'identity) a*)
             == a*
  Composition:  (amap (amap (amap (unit class (curry #'compose)) b->*c) a->*b) a*)
             == (amap b->*c (amap a->*b a*))
  Homomorphism: (amap (unit class #'a->b) (unit class a))
             == (unit class (a->b a))
  Interchange:  (amap a->*b (unit class a))
             == (amap (unit class (lambda (a->b) (funcall a->b a))) a->*b)"))

(defmacro define-fmap-by-applicative (class)
  `(defmethod fmap (a->b (a* ,class))
     (amap (unit ',class a->b) a*)))
