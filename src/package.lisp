(defpackage fcl
  (:use :common-lisp)
  (:export
    ;; Lazy
    #:delay
    #:force

    ;; DEFDATA
    #:defdata

    ;; Foldable
    #:foldr
    #:foldl
    #:foldr+
    #:foldl+
    #:unfoldr
    #:unfoldl
    #:unfoldr+
    #:unfoldl+

    ;; Monad
    #:unit
    #:fmap
    #:amap
    #:mmap

    ;; Monoid
    #:mzero
    #:mplus
    #:msum
    #:guard

    ;; Syntax Sugars
    #:monad-do
    #:mlet
    #:mprogn
    #:genlist))
