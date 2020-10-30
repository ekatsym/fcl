(defpackage fcl.datatypes.llist
  (:nicknames :fcl.dt.llist :fcl.llist)
  (:use
    :common-lisp
    :fcl.generics.foldable
    :fcl.generics.monad-plus)
  (:import-from
    :fcl.lazy
    #:delay
    #:force)
  (:import-from
    :fcl.data
    #:defdata)
  (:import-from
    :fcl.match
    #:ematch)
  (:import-from
    :fcl.datatypes.maybe
    #:maybe
    #:just #:just%0
    #:nothing)
  (:import-from
    :fcl.datatypes.either
    #:either
    #:left #:left%0
    #:right #:right%0)
  (:export
    ;; Core
    #:lcons
    #:lnil

    ;; CL-like Utility
    #:lconsp
    #:lnull
    #:lendp
    #:llist
    #:lcar
    #:lcdr
    #:lfirst
    #:lrest
    #:ladjoin
    #:lnth
    #:lnthcdr
    #:llast
    #:lbutlast
    #:lreverse
    #:lappend
    #:lrevappend
    #:llength
    #:lcount
    #:lcount-if
    #:lcount-if-not
    #:lremove
    #:lremove-if
    #:lremove-if-not
    #:lsubstitute
    #:lsubstitute-if
    #:lsubstitute-if-not
    #:lfind
    #:lfind-if
    #:lfind-if-not
    #:lposition
    #:lposition-if
    #:lposition-if-not
    #:lreplace
    #:lmapc
    #:lmapcar
    #:lmapcan
    #:lmapl
    #:lmaplist
    #:lmapcon
    #:lsearch
    #:lmismatch
    #:lsort

    ;; Lazy
    #:delay
    #:force

    ;; Foldable
    #:cata
    #:para
    #:ana
    #:apo
    #:foldr
    #:foldr+
    #:unfoldr
    #:unfoldr+
    #:foldl
    #:foldl+
    #:unfoldl
    #:unfoldl+
    #:foldt
    #:foldt+
    #:unfoldt
    #:unfoldt+
    #:scanr
    #:scanr+
    #:scanl
    #:scanl+
    #:scant
    #:scant+

    ;; Monad Plus
    #:fmap
    #:unit
    #:amap
    #:mmap
    #:mlet
    #:mprogn
    #:mdo
    #:mzero
    #:mplus
    #:msum
    #:guard
    #:llc))
(in-package :fcl.datatypes.llist)
