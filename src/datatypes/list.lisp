(defpackage fcl.datatypes.list
  (:nicknames :fcl.dt.list)
  (:use :common-lisp)
  (:import-from
    :fcl.monad
    #:unit
    #:fmap
    #:amap
    #:mmap)
  (:import-from
    :fcl.monoid
    #:mzero
    #:mplus))
(in-package :fcl.dt.list)


(defmethod unit ((class (eql 'list)) a)
  (list a))

(defmethod fmap (a->b (a* list))
  (check-type a->b function)
  (mapcar a->b a*))

(defmethod amap (a->*b (a* list))
  (check-type a->*b list)
  (every (lambda (a->b) (check-type a->b function)) a->*b)
  (reduce (lambda (a->b acc)
            (reduce (lambda (a acc) (cons (funcall a->b a) acc))
                    a*
                    :initial-value acc
                    :from-end t))
          a->*b
          :initial-value '()
          :from-end t))

(defmethod mmap (a->b* (a* list))
  (check-type a->b* function)
  (reduce (lambda (a acc) (append (funcall a->b* a) acc))
          a*
          :initial-value '()
          :from-end t))

(defmethod mzero ((class (eql 'list)))
  '())

(defmethod mplus ((monoid1 list) monoid2)
  (check-type monoid2 list)
  (append monoid1 monoid2))
