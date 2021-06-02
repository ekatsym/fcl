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
    #:define-fmap-by-applicative
    #:define-fmap-by-monad #:define-amap-by-monad
    #:guard
    #:mzero #:mplus #:msum))
(in-package :fcl.either)


;;; Definition
(defdata either
  (left t)
  (right t))


;;; Methods
(defmethod fmap (a->b (a* either))
  (check-type a->b function)
  (ematch a*
    ((left _)  a*)
    ((right a) (right (funcall a->b a)))))

(defmethod unit ((class (eql 'either)) a)
  (right a))

(defmethod amap (a->*b (a* either))
  (check-type a->*b either)
  (ematch a->*b
    ((left _)     a->*b)
    ((right a->b) (ematch a*
                    ((left _)  a*)
                    ((right a) (right (funcall a->b a)))))))

(defmethod mmap (a->b* (a* either))
  (check-type a->b* function)
  (ematch a*
    ((left _)  a*)
    ((right a) (funcall a->b* a))))

(defmethod mzero ((class (eql 'either)))
  (left nil))

(defmethod mplus ((monoid1 either) monoid2)
  (check-type monoid2 either)
  (ematch monoid1
    ((left _)   monoid2)
    ((right a1) (ematch monoid2
                  ((left _)   monoid1)
                  ((right a2) (right (mplus a1 a2)))))))
