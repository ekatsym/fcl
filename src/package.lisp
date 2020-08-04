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
    #:monad-do
    #:mlet
    #:mprogn

    ;; Monoid
    #:mzero
    #:mplus
    #:msum
    #:guard

    ;; List
    #:enum
    #:take
    #:drop
    #:genlist))
