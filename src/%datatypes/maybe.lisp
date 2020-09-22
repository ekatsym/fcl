(defpackage fcl.datatypes.maybe
  (:nicknames fcl.dt.maybe)
  (:use :common-lisp)
  (:import-from
    :fcl
    #:maybe
    #:nothing
    #:just)
  (:import-from
    :fcl.defdata
    #:defdata)
  (:import-from
    :fcl.monad
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:mprogn
    #:mlet
    #:monad-do)
  (:import-from
    :fcl.monoid
    #:mzero
    #:mplus
    #:msum)
  (:import-from
    :fcl.monad+
    #:guard)
  (:export
    #:maybe
    #:nothing
    #:just
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:mprogn
    #:mlet
    #:monad-do
    #:mzero
    #:mplus
    #:msum
    #:guard))

(in-package fcl.dt.maybe)


;;; Definition
(defdata maybe
  (nothing)
  (just t))


;;; Functor, Applicative and Monad
(defmethod unit ((class (eql 'maybe)) a)
  (just a))

(defmethod fmap (a->b (a* maybe))
  (declare (optimize (speed 3)))
  (check-type a->b function)
  (typecase a*
    (nothing   (nothing))
    (otherwise (just (funcall a->b (just%0 a*))))))

(defmethod amap (a->*b (a* maybe))
  (declare (optimize (speed 3)))
  (check-type a->*b maybe)
  (typecase a->*b
    (nothing   (nothing))
    (otherwise (typecase a*
                 (nothing   (nothing))
                 (otherwise (just (funcall (the function (just%0 a->*b))
                                           (just%0 a*))))))))

(defmethod mmap (a->b* (a* maybe))
  (declare (optimize (speed 3)))
  (check-type a->b* function)
  (typecase a*
    (nothing   (nothing))
    (otherwise (the (values maybe &optional) (funcall a->b* (just%0 a*))))))


;;; Monoid
(defmethod mzero ((class (eql 'maybe)))
  (nothing))

(defmethod mplus ((monoid1 maybe) monoid2)
  (check-type monoid2 maybe)
  (typecase monoid1
    (nothing   monoid2)
    (otherwise (typecase monoid2
                 (nothing   monoid1)
                 (otherwise (mlet ((x1 monoid1)
                                   (x2 monoid2))
                              (unit 'maybe (mplus x1 x2))))))))
