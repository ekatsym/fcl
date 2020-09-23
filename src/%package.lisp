(defpackage fcl.core
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
    #:mprogn
    #:mlet
    #:monad-do
    #:join-m
    #:sequence-m
    #:map-m
    #:map-m_
    #:for-m
    #:for-m_
    #:define-fmap-by-monad
    #:define-amap-by-monad

    ;; Monoid
    #:mzero
    #:mplus
    #:msum

    ;; Monad Plus
    #:guard))

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
    #:mprogn
    #:mlet
    #:monad-do
    #:join-m
    #:sequence-m
    #:map-m
    #:map-m_
    #:for-m
    #:for-m_
    #:define-fmap-by-monad
    #:define-amap-by-monad

    ;; Monoid
    #:mzero
    #:mplus
    #:msum

    ;; Monad Plus
    #:guard

    ;; List
    #:enum
    #:take
    #:drop
    #:sublist
    #:nlist?
    #:filter
    #:zip
    #:genlist

    ;; Function
    #:projection
    #:compose
    #:partial
    #:rpartial
    #:curry
    #:rcurry
    #:flip

    ;; Maybe
    #:maybe
    #:just
    #:nothing

    ;; Either
    #:either
    #:left
    #:right

    ;; Reader
    #:reader
    #:run-reader
    #:ask
    #:local
    #:asks

    ;; Writer
    #:writer
    #:run-writer
    #:pass
    #:hear
    #:tell
    #:hears
    #:censor

    ;; State
    #:state
    #:run-state
    #:get-state
    #:set-state
    #:modify-state

    ;; ST
    #:st
    #:run-st
    #:new-stref
    #:read-stref
    #:write-stref
    #:modify-stref

    ;; Lazy List
    #:lazy-list
    #:lnil
    #:lcons
    #:lfirst
    #:lrest
    #:lendp
    #:genllist
    #:llist
    #:lenum
    #:ladjoin
    #:ltake
    #:ldrop
    #:llast
    #:lbutlast
    #:lsublist
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
    #:lmember
    #:lmember-if
    #:lmember-if-not
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
    #:lsort))