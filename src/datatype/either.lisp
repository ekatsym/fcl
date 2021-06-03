(defpackage fcl.either
  (:nicknames :fcl.data.either :fcl.et)
  (:use :common-lisp :fcl.monad-plus)
  (:import-from
    :fcl.adata
    #:defdata)
  (:import-from
    :fcl.match
    #:ematch)
  (:export
    #:either #:left #:right
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:mzero #:mplus #:msum
    #:guard))
(in-package :fcl.either)


;;; Definition
(defdata either
  (left t)
  (right t))


;;; MONAD-PLUS
(defmethod unit ((class (eql 'either)) a)
  (right a))

(define-fmap-by-monad either)

(define-amap-by-monad either)

(defmethod mmap (a->b* (a* either))
  (check-type a->b* function)
  (ematch a*
    ((left _)  a*)
    ((right a) (funcall a->b* a))))

(defmethod mzero ((class (eql 'either)))
  (left nil))

(defmethod mplus ((monoid1 either) monoid2)
  (check-type monoid2 either)
  (ematch (cons monoid1 monoid2)
    ((cons (left _) _)            monoid2)
    ((cons _ (left _))            monoid1)
    ((cons (right a1) (right a2)) (right (mplus a1 a2)))))

