(defpackage fcl.datatypes.either
  (:nicknames :fcl.dt.either :fcl.either)
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
    #:either
    #:left #:left%0
    #:right #:right%0

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
(in-package :fcl.datatypes.either)


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
