(defpackage fcl
  (:use :common-lisp)
  (:export
    ;; Lazy
    #:delay
    #:force

    ;; DEFDATA
    #:defdata

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
