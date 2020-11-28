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
    #:match
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
    #:lnil
    #:lcons
    #:lcons%0
    #:lcons%1

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


;;; Definition
(defdata llist
  (lnil)
  (lcons (:lazy t) (:lazy llist)))


;;; Printer
(defmethod print-object ((object lnil) stream)
  (format stream "#.(LNIL)"))

(defmethod print-object ((object lcons) stream)
  (match object
    ((lcons x xs)  (format stream "#.(LCONS ~S ~S)" x xs))))


;;; Foldable
(defmethod cata (x*->x (i llist))
  "X* == (NOTHING) | (JUST (LIST A X))"
  (check-type x*->x function)
  (ematch i
    ((lnil)       (funcall x*->x (nothing)))
    ((lcons a as) (funcall x*->x (just (list a (cata x*->x as)))))))

(defmethod para (i&*x->x (i llist))
  "I&*X == (NOTHING) | (JUST (LIST A I X))"
  (check-type i&*x->x function)
  (ematch i
    ((lnil)       (funcall i&*x->x (nothing)))
    ((lcons a as) (funcall i&*x->x (just (list a i (para i&*x->x as)))))))

(defmethod ana ((class (eql 'llist)) x->x* x)
  "X* == (NOTHING) | (JUST (LIST A X))"
  (check-type x->x* function)
  (ematch (funcall x->x* x)
    ((nothing)         (lnil))
    ((just (list a x)) (lcons a (ana 'llist x->x* x)))))

(defmethod apo ((class (eql 'llist)) x->f+*x x)
  "F+*X == (NOTHING) | (JUST (LIST A (LEFT F))) | (JUST (LIST A (RIGHT X)))"
  (check-type x->f+*x function)
  (ematch (funcall x->f+*x x)
    ((nothing)           (lnil))
    ((just (list a f+x)) (ematch f+x
                           ((left f)  (lcons a f))
                           ((right x) (lcons a (apo 'llist x->f+*x x)))))))

(defmethod foldr (a&x->x x0 (as llist))
  (check-type a&x->x function)
  (ematch as
    ((lnil)       x0)
    ((lcons a as) (funcall a&x->x a (foldr a&x->x x0 as)))))

(defmethod foldr+ (as&x->x x0 (as llist))
  (check-type as&x->x function)
  (ematch as
    ((lnil) x0)
    ((lcons _ as2) (funcall as&x->x as (foldr+ as&x->x x0 as2)))))

(defmethod unfoldr ((class (eql 'llist)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (if (funcall x->? x)
      (lnil)
      (lcons (funcall x->a x) (unfoldr 'llist x->? x->a x->x (funcall x->x x)))))

(defmethod unfoldr+ ((class (eql 'llist)) x->? x->a x->x as0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (if (funcall x->? x)
      as0
      (lcons (funcall x->a x) (unfoldr 'llist x->? x->a x->x (funcall x->x x)))))

(defmethod foldl (x&a->x x0 (as llist))
  (check-type x&a->x function)
  (labels ((rec (as x)
             (declare (optimize (speed 3)))
             (ematch as
               ((lnil)       x)
               ((lcons a as) (rec as (funcall x&a->x x a))))))
    (rec as x0)))

(defmethod foldl+ (x&as->x x0 (as llist))
  (check-type x&as->x function)
  (labels ((rec (as x)
             (declare (optimize (speed 3)))
             (ematch as
               ((lnil) x)
               ((lcons _ as2) (rec as2 (funcall x&as->x x as))))))
    (rec as x0)))

(defmethod unfoldl ((class (eql 'llist)) x->? x->x x->a x)
  (check-type x->? function)
  (check-type x->x function)
  (check-type x->a function)
  (labels ((rec (x as)
             (declare (optimize (speed 3))
                      (type function x->? x->x x->a))
             (if (funcall x->? x)
                 as
                 (rec (funcall x->x x) (lcons (funcall x->a x) as)))))
    (rec x (lnil))))

(defmethod unfoldl+ ((class (eql 'llist)) x->? x->x x->a as0 x)
  (check-type x->? function)
  (check-type x->x function)
  (check-type x->a function)
  (check-type as0 llist)
  (labels ((rec (x as)
             (declare (optimize (speed 3))
                      (type function x->? x->x x->a))
             (if (funcall x->? x)
                 as
                 (rec (funcall x->x x) (lcons (funcall x->a x) as)))))
    (rec x as0)))

(defmethod foldt (a&xs->x x0 (at llist))
  (check-type a&xs->x function)
  (labels ((rec (at)
             (ematch at
               ((lnil)        x0)
               ((lcons a ats) (funcall a&xs->x a (mapcar #'rec ats))))))
    (rec at)))

(defmethod foldt+ (at&xs->x x0 (at llist))
  (check-type at&xs->x function)
  (labels ((rec (at)
             (ematch at
               ((lnil)         x0)
               ((lcons _ ats) (funcall at&xs->x at (mapcar #'rec ats))))))
    (rec at)))

(defmethod unfoldt ((class (eql 'llist)) x->? x->a x->xs x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->xs function)
  (labels ((rec (x)
             (if (funcall x->? x)
                 (lnil)
                 (lcons (funcall x->a x) (funcall x->xs x)))))
    (rec x)))

(defmethod unfoldt+ ((class (eql 'llist)) x->? x->a x->xs at0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->xs function)
  (check-type at0 llist)
  (labels ((rec (x)
             (if (funcall x->? x)
                 at0
                 (lcons (funcall x->a x) (funcall x->xs x)))))
    (rec x)))
