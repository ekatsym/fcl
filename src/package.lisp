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

    ;; Syntax Sugars
    #:monad-do
    #:mlet
    #:mprogn
    #:genlist))

(defpackage fcl.sugar
  (:use :common-lisp)
  (:import-from
    :fcl
    #:monad-do
    #:mlet
    #:mprogn
    #:genlist)
  (:export
    #:monad-do
    #:mlet
    #:mprogn
    #:genlist))
