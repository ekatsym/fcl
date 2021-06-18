(defpackage fcl.promise
  (:nicknames :fcl.data.promise :fcl.pm)
  (:use :common-lisp :fcl.lazy :fcl.monad)
  (:export
    #:promise #:delay #:force
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo))
(in-package :fcl.promise)


;;; MONAD
(defmethod unit ((class (eql 'promise)) a)
  (delay a))

(define-fmap-by-monad promise)

(define-amap-by-monad promise)

(defmethod mmap (a->b* (a* promise))
  (check-type a->b* function)
  (funcall a->b* (force a*)))

