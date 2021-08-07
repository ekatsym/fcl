(defpackage fcl.applicative
  (:nicknames :fcl.generics.applicative :fcl.ap)
  (:use :common-lisp :fcl.functor)
  (:import-from :fcl.util #:partial #:curry)
  (:export
    #:unit #:fmap #:amap
    #:lift1 #:lift2 #:liftn
    #:define-fmap-by-applicative))
(in-package :fcl.applicative)


;;; Core
(defgeneric unit (class a)
  (:documentation
"Returns a minimal unit value of CLASS including A."))

(defgeneric amap (a->*b a*)
  (:documentation
"Returns a value of class B* including (FUNCALL A->B A)
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


;;; Lifts
(defun lift1 (a->b a*)
  (check-type a->b function)
  (fmap f->b a*))

(defun lift2 (a&b->c a* b*)
  (check-type a&b->c function)
  (amap (fmap (curry a&b->c) a*) b*))

(defun liftn (a1&-&an->b a1* &rest a2*-an*)
  (check-type a1&-&an->b function)
  (fmap #'funcall
        (reduce (lambda (ai&-&an->*b ai*) (lift2 #'partial ai&-&an->*b ai*))
                a2*-an*
                :initial-value (fmap (curry a1&-&an->b) a1*))))


;;; Shorthand for Functor
(defmacro define-fmap-by-applicative (class)
  `(defmethod fmap (a->b (a* ,class))
     (amap (unit ',class a->b) a*)))
