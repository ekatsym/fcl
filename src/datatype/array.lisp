(defpackage fcl.array
  (:nicknames :fcl.data.array :fcl.ar)
  (:use :common-lisp)
  (:export
    #:ac

    ;; monad-plus
    #:guard
    #:mmap
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-monad #:define-amap-by-monad
    #:unit #:amap
    #:define-fmap-by-applicative
    #:fmap
    #:mzero #:mplus #:msum

    ;; foldable
    #:foldr #:foldr+ #:unfoldr #:unfoldr+
    #:foldl #:foldl+ #:unfoldl #:unfoldl+
    #:lfoldr #:lfoldr+
    #:lfoldl #:lfoldl+
    #:scanr #:scanr+ #:scanl #:scanl+))
(in-package :fcl.array)
