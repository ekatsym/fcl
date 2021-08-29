(defpackage fcl.traversable
  (:nicknames :fcl.generics.traversable :fcl.tv)
  (:use :common-lisp :fcl.foldable :fcl.monad)
  (:import-from :fcl.util #:partial #:curry)
  (:export
    ;; Core
    #:traverse

    ;; Monad
    #:unit #:fmap #:amap #:mmap
    #:lift1 #:lift2 #:liftn
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-applicative

    ;; Foldable
    #:foldr #:foldr+ #:foldl #:foldl+
    #:delay #:force
    #:lfoldr #:lfoldr+ #:lfoldl #:lfoldl+
    #:scanr #:scanr+ #:scanl #:scanl+))
(in-package :fcl.traversable)


#|
(defun traverse (tvclass apclass a->b* as)
  (check-type a->b* function)
  (foldr (lambda (a bs*) (lift2 (partial #'add tvclass) (funcall a->b* a) bs*))
         (unit apclass (empty tvclass))
         as))

(defun sequential (tvclass apclass a*s)
  (foldr (lambda (a* as*) (lift2 (partial #'add tvclass) a* as*))
         (unit apclass (empty tvclass))
         a*s))
|#
