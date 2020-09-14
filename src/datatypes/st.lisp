(defpackage fcl.datatypes.st
  (:nicknames :fcl.dt.st)
  (:use :common-lisp)
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
  (:export
    #:st
    #:run-st
    #:unit
    #:mmap
    #:mprogn
    #:mlet
    #:monad-do
    #:new-stref
    #:read-stref
    #:write-stref
    #:modify-stref))

(in-package :fcl.dt.st)


;;; Definition
(defdata st
  (%st function))

(defun run-st (st)
  (check-type st st)
  (funcall (%st%0 st) (make-hash-table :test #'eq)))

(defun %run-st (st s)
  (check-type st st)
  (funcall (%st%0 st) s))


;;; Functor, Applicative and Monad
(defmethod unit ((class (eql 'st)) a)
  (%st (lambda (s) (values a s))))

(defmethod fmap (a->b (a* st))
  (check-type a->b function)
  (%st
    (lambda (s0)
      (multiple-value-bind (a s1) (%run-st a* s0)
        (values (funcall a->b a) s1)))))

(defmethod amap (a->*b (a* st))
  (check-type a->*b st)
  (%st
    (lambda (s0)
      (multiple-value-bind (a->b s1) (%run-st a->*b s0)
        (multiple-value-bind (a s2) (%run-st a* s1)
          (values (funcall a->b a) s2))))))

(defmethod mmap (a->b* (a* st))
  (check-type a->b* function)
  (%st
    (lambda (s0)
      (multiple-value-bind (a s1) (%run-st a* s0)
        (%run-st (funcall a->b* a) s1)))))


;;; General Utility
(defun new-stref (a)
  (%st (lambda (s)
         (let ((stref (gensym "STREF")))
           (setf (gethash stref s) a)
           (values stref s)))))

(defun read-stref (stref)
  (check-type stref symbol)
  (%st (lambda (s) (values (gethash stref s) s))))

(defun write-stref (stref a)
  (check-type stref symbol)
  (%st (lambda (s)
         (setf (gethash stref s) a)
         (values nil s))))

(defun modify-stref (stref f)
  (check-type stref symbol)
  (check-type f function)
  (%st (lambda (s)
         (setf (gethash stref s) (funcall f (gethash stref s)))
         (values nil s))))
