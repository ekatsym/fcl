(defpackage fcl.function
  (:nicknames :fcl.data.function :fcl.fn)
  (:use :common-lisp :fcl.monad)
  (:export
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo))
(in-package :fcl.function)


;;; MONAD-PLUS
(defmethod unit ((class (eql 'function)) a)
  (constantly a))

(define-fmap-by-monad function)

(define-amap-by-monad function)

(defmethod mmap (a->b* (a* function))
  (check-type a->b* function)
  (lambda (s) (funcall (funcall a->b* (funcall a* s)) s)))
