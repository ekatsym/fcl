(defpackage fcl.datatypes.maybe
  (:nicknames :fcl.dt.maybe :fcl.maybe)
  (:use
    :common-lisp
    :fcl.generics.monad-plus)
  (:import-from
    :fcl.data
    #:defdata)
  (:import-from
    :fcl.match
    #:ematch)
  (:export
    #:maybe
    #:just #:just%0
    #:nothing

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
    #:guard))
(in-package :fcl.datatypes.maybe)


;;; Definition
(defdata maybe
  (just t)
  (nothing))


;;; Methods
(defmethod fmap (a->b (a* maybe))
  (check-type a->b function)
  (ematch a*
    ((nothing) a*)
    ((just a)  (just (funcall a->b a)))))

(defmethod unit ((class (eql 'maybe)) a)
  (just a))

(defmethod amap (a->*b (a* maybe))
  (check-type a->*b maybe)
  (ematch a->*b
    ((nothing)   a->*b)
    ((just a->b) (ematch a*
                   ((nothing) a*)
                   ((just a)  (just (funcall a->b a)))))))

(defmethod mmap (a->b* (a* maybe))
  (check-type a->b* function)
  (ematch a*
    ((nothing) a*)
    ((just a)  (funcall a->b* a))))

(defmethod mzero ((class (eql 'maybe)))
  (nothing))

(defmethod mplus ((monoid1 maybe) monoid2)
  (check-type monoid2 maybe)
  (ematch monoid1
    ((nothing) monoid2)
    ((just a1) (ematch monoid2
                 ((nothing) monoid1)
                 ((just a2) (just (mplus a1 a2)))))))
