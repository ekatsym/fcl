(defpackage fcl.datatypes.either
  (:nicknames fcl.dt.either)
  (:use :common-lisp)
  (:import-from
    :fcl
    #:either
    #:left
    #:right)
  (:import-from
    :fcl.defdata
    #:defdata)
  (:import-from
    :fcl.monad
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet)
  (:import-from
    :fcl.monoid
    #:mzero
    #:mplus
    #:msum)
  (:import-from
    :fcl.monad+
    #:guard)
  (:export
    #:either
    #:left
    #:right
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet
    #:mzero
    #:mplus
    #:msum
    #:guard))

(in-package :fcl.dt.either)


;;; Definition
(defdata either
  (left t)
  (right t))


;;; Functor, Applicative and Monad
(defmethod unit ((class (eql 'either)) a)
  (right a))

(defmethod fmap (a->b (a* either))
  (declare (optimize (speed 3)))
  (check-type a->b function)
  (typecase a*
    (left      a*)
    (otherwise (right (funcall a->b (right%0 a*))))))

(defmethod amap (a->*b (a* either))
  (declare (optimize (speed 3)))
  (check-type a->*b either)
  (typecase a->*b
    (left      (typecase a*
                 (left      (mplus a->*b a*))
                 (otherwise a->*b)))
    (otherwise (typecase a*
                 (left      a->*b)
                 (otherwise (funcall (the function (right%0 a->*b))
                                     (right%0 a*)))))))

(defmethod mmap (a->b* (a* either))
  (declare (optimize (speed 3)))
  (check-type a->b* function)
  (typecase a*
    (left      a*)
    (otherwise (the (values either &optional) (funcall a->b* (right%0 a*))))))


;;; Monoid
(defmethod mzero ((class (eql 'either)))
  (left nil))

(defmethod mplus ((monoid1 either) monoid2)
  (check-type monoid2 either)
  (typecase monoid1
    (left      (typecase monoid2
                 (left      (left (mplus (left%0 monoid1) (left%0 monoid2))))
                 (otherwise monoid2)))
    (otherwise (typecase monoid2
                 (left      monoid1)
                 (otherwise (right (mplus (right%0 monoid1) (right%0 monoid2))))))))
