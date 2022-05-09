(defpackage fcl.llist
  (:nicknames :fcl.data.llist :fcl.ll)
  (:use :common-lisp :fcl.monad-plus :fcl.foldable :fcl.unfoldable)
  (:import-from :fcl.adata #:defdata #:data=)
  (:import-from :fcl.match #:match #:ematch)
  (:export
    ;; Core
    #:llist
    #:ll-nil #:ll-null #:ll-endp
    #:ll-cons #:ll-car #:ll-first #:ll-cdr #:ll-rest
    #:ll-list
    #:ll-list? #:ll-listp
    #:data=

    ;; LIST Convertions
    #:llist->list
    #:list->llist

    ;; Foldable
    #:foldr #:foldr+
    #:foldl #:foldl+
    #:lfoldr #:lfoldr+
    #:lfoldl #:lfoldl+
    #:scanr #:scanr+ #:scanl #:scanl+

    ;; Unfoldable
    #:unfoldr #:unfoldr+
    #:unfoldl #:unfoldl+

    ;; Monad Plus
    #:unit #:fmap #:amap #:mmap
    #:lift1 #:lift2 #:liftn
    #:mlet #:mprogn #:mdo
    #:mzero #:mplus #:msum
    #:guard

    ;; CL-like Utility
    #:ll-consp
    #:ll-copy-list
    #:ll-list*
    #:ll-make-list
    #:ll-nth
    #:ll-append
    #:ll-revappend
    #:ll-last
    #:ll-butlast
    #:ll-ldiff #:ll-tailp
    #:ll-nthcdr
    #:ll-member #:ll-member-if #:ll-member-if-not
    #:ll-mapcar #:ll-mapcan #:ll-maplist #:ll-mapcon
    #:ll-acons
    #:ll-assoc #:ll-assoc-if #:ll-assoc-if-not
    #:ll-copy-alist
    #:ll-pairlis
    #:ll-rassoc #:ll-rassoc-if #:rassoc-if-not
    #:ll-intersection
    #:ll-adjoin
    #:ll-set-difference
    #:ll-set-exclusive-or
    #:ll-subsetp
    #:ll-union
    #:ll-fill
    #:ll-subseq
    #:ll-reduce
    #:ll-count #:ll-count-if #:ll-count-if-not
    #:ll-length
    #:ll-reverse
    #:ll-sort #:ll-stable-sort
    #:ll-find #:ll-find-if #:ll-find-if-not
    #:ll-position #:ll-position-if #:ll-position-if-not
    #:ll-search
    #:ll-mismatch
    #:ll-replace
    #:ll-substitute #:ll-substitute-if #:ll-remove-if-not
    #:ll-remove-duplicates

    ;; Alexandria-like Utility
    #:ll-flatten
    #:ll-lastcar
    #:ll-map-product
    #:ll-set-equal
    #:ll-setp
    #:ll-rotate
    #:ll-shuffle
    #:ll-random-elt
    #:ll-emptyp
    #:ll-sequence-of-length-p
    #:ll-length=
    #:ll-starts-with #:ll-starts-with-subseq
    #:ll-ends-with #:ll-ends-with-subseq
    #:ll-map-combinations
    #:ll-map-derangements
    #:ll-map-permutations))
(in-package :fcl.llist)


;;; Core
(defdata llist
  (ll-nil)
  (ll-cons (:lazy t) (:lazy llist)))

(defun ll-null (object)
  (typep object 'll-nil))

(defun ll-endp (llist)
  (check-type llist llist)
  (typep llist 'll-nil))

(defun ll-first (ll-cons)
  (check-type ll-cons ll-cons)
  (ematch ll-cons
    ((ll-cons x _) x)))

(defun ll-car (ll-cons)
  (declare (inline))
  (ll-first ll-cons))

(defun ll-rest (ll-cons)
  (check-type ll-cons ll-cons)
  (ematch ll-cons
    ((ll-cons _ x) x)))

(defun ll-cdr (ll-cons)
  (declare (inline))
  (ll-rest ll-cons))

(defun ll-list (&rest args)
  (list->llist args))

(defun llist? (object)
  (typep object 'llist))

(defun ll-listp (object)
  (declare (inline))
  (llist? object))


;;; LIST Convertions
(defun llist->list (llist)
  (check-type llist llist)
  (do ((llst llist (ll-rest llst))
       (acc '() (cons (ll-first llst) acc)))
      ((ll-endp llst) (nreverse acc))))

(defun list->llist (list)
  (check-type list list)
  (if (endp list)
      (ll-nil)
      (ll-cons (first list) (list->llist (rest list)))))


;;; Printer
(defmethod print-object ((object ll-nil) stream)
  (format stream "#<LLIST ~S>" (llist->list object)))

(defmethod print-object ((object ll-cons) stream)
  (format stream "#<LLIST ~S>" (llist->list object)))


