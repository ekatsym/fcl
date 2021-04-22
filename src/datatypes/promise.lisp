(defpackage fcl.datatypes.promise
  (:nicknames :fcl.dt.promise :fcl.promise)
  (:use
    :common-lisp
    :fcl.lazy
    :fcl.generics.monad)
  (:import-from
    :fcl.match
    #:ematch)
  (:export
    #:promise
    #:delay
    #:force

    #:fmap
    #:unit
    #:amap
    #:mmap
    #:mlet
    #:mprogn
    #:mdo))
(in-package :fcl.datatypes.promise)


;;; Monad Plus
(defmethod unit ((class (eql 'promise)) a)
  (delay a))

(defmethod mmap (a->b* (a* promise))
  (check-type a->b* function)
  (funcall a->b* (force a*)))

(define-fmap-by-monad promise)
(define-amap-by-monad promise)
