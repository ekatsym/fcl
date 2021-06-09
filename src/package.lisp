(defpackage fcl
  (:use
    :common-lisp
    ;; core
    :fcl.adata
    :fcl.lazy
    :fcl.match

    ;; generics
    :fcl.functor
    :fcl.applicative
    :fcl.monad
    :fcl.monoid
    :fcl.monad-plus
    :fcl.foldable

    ;; data
    :fcl.promise
    :fcl.maybe
    :fcl.either
    :fcl.list
    :fcl.vector
    :fcl.array
    :fcl.function
    :fcl.reader
    :fcl.writer
    :fcl.state)
  (:export
    #:algebraic-data
    #:defdata
    #:data=
    #:promise #:delay #:force
    #:match #:ematch
    #:quote #:cons #:list #:vector
    #:fmap
    #:unit #:amap
    #:define-fmap-by-applicative
    #:mmap
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-monad #:define-amap-by-monad
    #:mzero #:mplus #:msum
    #:guard
    #:cata #:para #:ana #:apo
    #:foldr #:foldr+ #:unfoldr #:unfoldr+
    #:foldl #:foldl+ #:unfoldl #:unfoldl+
    #:lfoldr #:lfoldr+
    #:lfoldl #:lfoldl+
    #:scanr #:scanr+ #:scanl #:scanl+
    #:maybe #:nothing #:just
    #:either #:left #:right
    #:lc
    #:vc
    #:ac
    #:reader #:run-reader #:getrd #:local
    #:writer #:run-writer #:setw #:getw #:modw
    #:state #:run-state #:getst #:setst #:dost))
