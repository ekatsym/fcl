(defpackage fcl.writer
  (:nicknames :fcl.data.writer :fcl.wt)
  (:use :common-lisp :fcl.monad-plus)
  (:export
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-applicative
    #:define-fmap-by-monad
    #:define-amap-by-monad
    #:guard
    #:mzero #:mplus #:msum))
(in-package :fcl.writer)
