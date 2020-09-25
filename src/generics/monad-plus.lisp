(defpackage fcl.generics.monad-plus
  (:nicknames :fcl.g.monad-plus :fcl.monad-plus :fcl.g.monad+ :fcl.monad+)
  (:use
    :common-lisp
    :fcl.generics.monad
    :fcl.generics.monoid)
  (:export
    #:fmap
    #:unit
    #:amap
    #:mmap
    #:mlet
    #:mprogn
    #:mdo
    #:define-fmap-by-applicative
    #:define-fmap-by-monad
    #:define-amap-by-monad
    #:mzero
    #:mplus
    #:msum
    #:guard))
(in-package :fcl.generics.monad-plus)


(defun guard (class test)
  (if test
      (unit class nil)
      (mzero class)))
