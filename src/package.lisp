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
    #:guard

    ;; List
    #:enum
    #:take
    #:drop
    #:sublist
    #:nlist?
    #:filter
    #:genlist

    ;; Function
    #:constant
    #:projection
    #:compose
    #:partial
    #:rpartial
    #:curry
    #:rcurry

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
    #:put-state
    #:modify-state
    #:pop-state
    #:push-state

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
