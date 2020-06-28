(defpackage fcl.datatypes.llist
  (:nicknames :fcl.dt.llist)
  (:use :common-lisp)
  (:import-from
    :fcl.defdata
    #:defdata)
  (:import-from
    :fcl.monad
    #:unit
    #:fmap
    #:amap
    #:mmap)
  (:import-from
    :fcl.monoid
    #:mzero
    #:mplus)
  (:export
    #:lazy-list
    )
  )
(in-package :fcl.dt.llist)


(defdata lazy-list
  (lnil)
  (lcons (:lazy t) (:lazy lazy-list)))

(declaim (inline lfirst))
(defun lfirst (llist)
  (check-type llist lazy-list)
  (lcons%0 llist))

(declaim (inline lrest))
(defun lrest (llist)
  (check-type llist lazy-list)
  (lcons%1 llist))

(declaim (inline lendp))
(defun lendp (llist)
  (check-type llist lazy-list)
  (typep llist 'lnil))

(defun lmap (function llist &rest more-llists)
  (check-type function function)
  (check-type llist lazy-list)
  (every (lambda (llist) (check-type llist lazy-list)) more-llists)
  (labels ((rec (llsts)
             (if (some #'lendp llsts)
                 (lnil)
                 (lcons (apply function (mapcar #'lfirst llsts))
                        (rec (mapcar #'lrest llsts))))))
    (rec (cons llist more-llists))))

(defmethod unit ((class (eql 'lazy-list)) a)
  (lcons a (lnil)))

(defmethod fmap (a->b (a* lazy-list))
  (check-type a->b function)
  (typecase a*
    (lnil      (lnil))
    (otherwise (lcons (funcall a->b (lfirst a*))
                      (fmap a->b (lrest a*))))))
