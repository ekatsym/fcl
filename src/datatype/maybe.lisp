(defpackage fcl.maybe
  (:nicknames :fcl.data.maybe :fcl.mb)
  (:use :common-lisp :fcl.monad-plus)
  (:import-from
    :fcl.adata
    #:defdata)
  (:import-from
    :fcl.match
    #:ematch)
  (:export
    #:maybe #:nothing #:just
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-applicative
    #:define-fmap-by-monad
    #:define-amap-by-monad
    #:guard
    #:mzero #:mplus #:msum))
(in-package :fcl.maybe)


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
