(defpackage fcl.monad-plus
  (:nicknames :fcl.generics.monad-plus :fcl.m+)
  (:use :common-lisp :fcl.monad :fcl.monoid)
  (:export
    #:unit #:fmap #:amap #:mmap
    #:lift1 #:lift2 #:liftn
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-applicative
    #:define-fmap-by-monad
    #:define-amap-by-monad
    #:mzero #:mplus #:msum
    #:guard))
(in-package :fcl.monad-plus)


(defun guard (class test)
  (if test
      (unit class nil)
      (mzero class)))
