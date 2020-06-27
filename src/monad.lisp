(defpackage fcl.monad
  (:use :common-lisp)
  (:import-from
    :fcl
    #:unit
    #:fmap
    #:amap
    #:mmap)
  (:export
    #:unit
    #:fmap
    #:amap
    #:mmap))
(in-package :fcl.monad)


(defgeneric unit (class a)
  (:documentation
"Returns a minimal unit value of CLASS including A."))

;;; Functor
(defgeneric fmap (a->b a*)
  (:documentation
"Returns a value of class of A* including (FUNCALL A->B A)
where A is a value included in A*.
FMAP must satisfy the rules:
  Identity:    (fmap #'identity a*)
               == (identity a*)
  Composition: (fmap (lambda (a) (b->c (a->b a))) a*)
               == (fmap #'b->c (fmap #'a->b a*))"))

;;; Applicative
(defgeneric amap (a->*b a*)
  (:documentation
"Returns a value of class of A* including (FUNCALL A->B A)
where A->B and A are values included A->*B and A*.
AMAP must satisfy the rules:
  Identity:     (amap (unit class #'identity) a*)
                == a*
  Composition:  (amap (amap (amap (unit class (curry #'compose)) b->*c) a->*b) #'a*)
                == (amap b->*c (amap a->*b #'a*))
  Homomorphism: (amap (unit class #'a->b) (unit class a))
                == (unit class (a->b a))
  Interchange:  (amap a->*b (unit class a))
                == (amap (unit class (lambda (a->b) (funcall a->b a))) a->*b)"))

;;; Monad
(defgeneric mmap (a->b* a*)
  (:documentation
"Returns a value of class of A*, JOINed (FMAP A->B* A)
where A is a value included A*.
MMAP must satisfy the rules:
  Left identity:  (mmap #'a->b* (unit class a))
                  == (a->b* a)
  Right identity: (mmap (partial #'unit class) a*)
                  == a*
  Associativity:  (mmap (lambda (a) (mmap #'b->c* (a->b* a))) a*)
                  == (mmap #'b->c* (mmap #'a->b* a*))"))
