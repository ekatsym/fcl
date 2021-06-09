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
    #:mzero #:mplus #:msum
    #:guard))
(in-package :fcl.maybe)


;;; Definition
(defdata maybe
  (just t)
  (nothing))


;;; MONAD-PLUS
(defmethod unit ((class (eql 'maybe)) a)
  (just a))

(define-fmap-by-monad maybe)

(define-amap-by-monad maybe)

(defmethod mmap (a->b* (a* maybe))
  (check-type a->b* function)
  (ematch a*
    ((nothing) a*)
    ((just a)  (funcall a->b* a))))

(defmethod mzero ((class (eql 'maybe)))
  (nothing))

(defmethod mplus ((monoid1 maybe) (monoid2 maybe))
  (ematch (cons monoid1 monoid2)
    ((cons (nothing) _)         monoid2)
    ((cons _ (nothing))         monoid1)
    ((cons (just a1) (just a2)) (just (mplus a1 a2)))))

