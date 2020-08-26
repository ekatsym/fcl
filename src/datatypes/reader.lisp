(defpackage fcl.datatypes.reader
  (:nicknames fcl.dt.reader)
  (:use :common-lisp)
  (:import-from
    :fcl
    #:reader
    #:run-reader
    #:ask
    #:local
    #:asks)
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
    #:reader
    #:run-reader
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
    #:guard
    #:ask
    #:local
    #:asks))

(in-package fcl.dt.reader)


;;; Definition
(defdata reader
  (%reader function))

(defun run-reader (reader r)
  (check-type reader reader)
  (funcall (%reader%0 reader) r))


;;; Functor, Applicative and Monad
(defmethod unit ((class (eql 'reader)) a)
  (%reader (lambda (r) (declare (ignore r)) a)))

(defmethod fmap (a->b (a* reader))
  (declare (optimize (speed 3)))
  (check-type a->b function)
  (%reader
    (lambda (r)
      (declare (type function a->b))
      (funcall a->b (run-reader a* r)))))

(defmethod amap (a->*b (a* reader))
  (declare (optimize (speed 3)))
  (check-type a->*b reader)
  (%reader
    (lambda (r)
      (funcall (the function (run-reader a->*b r)) (run-reader a* r)))))

(defmethod mmap (a->b* (a* reader))
  (declare (optimize (speed 3)))
  (check-type a->b* function)
  (%reader
    (lambda (r)
      (run-reader
        (the reader (funcall (the function a->b*) (run-reader a* r))) r))))


;;; Monoid
(defmethod mzero ((class (eql 'reader)))
  (unit 'reader nil))

(defmethod mplus ((monoid1 reader) monoid2)
  (check-type monoid2 reader)
  (mlet ((a1 monoid1)
         (a2 monoid2))
    (unit 'reader (mplus a1 a2))))


;;; General Utility
(defun ask ()
  (%reader #'identity))

(defun local (r->r reader)
  (check-type r->r function)
  (check-type reader reader)
  (%reader
    (lambda (r)
      (run-reader reader (funcall r->r r)))))

(defun asks (r->a)
  (check-type r->a function)
  (mlet ((a (ask)))
    (unit 'reader (funcall r->a a))))
