(defpackage fcl
  (:use
    :common-lisp
    ;; core
    :fcl.adata
    :fcl.delay
    :fcl.match

    ;; generics
    :fcl.functor
    :fcl.applicative
    :fcl.monad
    :fcl.monoid
    :fcl.monad-plus
    :fcl.recursive
    :fcl.foldable

    ;; data
    :fcl.promise
    :fcl.maybe
    :fcl.either
    :fcl.list
    :fcl.vector
    :fcl.array
    :fcl.llist
    :fcl.queue
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
    #:llist #:lnil #:lcons
    #:lapply
    #:lconsp #:lnull #:lendp
    #:lcar #:lcdr
    #:lfirst #:lrest
    #:lconsf #:ladjoin
    #:lnth #:lnthcdr #:llast #:lbutlast
    #:lappend #:lreverse #:lrevappend
    #:llength
    #:lcount #:lcount-if #:lcount-if-not
    #:lremove #:lremove-if #:lremove-if-not
    #:lsubstitute #:lsubstitute-if #:lsubstitute-if-not
    #:lfind #:lfind-if #:lfind-if-not
    #:lmember #:lmember-if #:lmember-if-not
    #:lposition #:lposition-if #:lposition-if-not
    #:lmapc #:lmapcar #:lmapcan
    #:lmapl #:lmaplist #:lmapcon
    #:lsearch #:lmismatch
    #:lsort #:lstable-sort
    #:llc
    #:queue
    #:enqueue
    #:dequeue))
