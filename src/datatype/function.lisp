(defpackage fcl.function
  (:nicknames :fcl.data.function :fcl.fn)
  (:use :common-lisp :fcl.monad-plus)
  (:export
    ;; monad-plus
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-applicative
    #:define-fmap-by-monad
    #:define-amap-by-monad
    #:guard
    #:mzero #:mplus #:msum))
(in-package :fcl.function)
