(defpackage fcl.promise
  (:nicknames :fcl.data.promise :fcl.pm)
  (:use :common-lisp :fcl.lazy :fcl.monad)
  (:export
    #:promise #:delay #:force
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-monad #:define-amap-by-monad
    #:define-fmap-by-applicative))
(in-package :fcl.promise)


;;; Monad Plus
(defmethod unit ((class (eql 'promise)) a)
  (delay a))

(defmethod mmap (a->b* (a* promise))
  (check-type a->b* function)
  (funcall a->b* (force a*)))

(define-fmap-by-monad promise)
(define-amap-by-monad promise)
