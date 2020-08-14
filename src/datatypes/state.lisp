(defpackage fcl.datatypes.state
  (:nicknames fcl.dt.state)
  (:use :common-lisp)
  (:import-from
    :fcl
    #:state
    #:run-state
    #:get-state
    #:put-state
    #:modify-state
    #:pop-state
    #:push-state)
  (:import-from
    :fcl.defdata
    #:defdata)
  (:import-from
    :fcl.monad
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet)
  (:import-from
    :fcl.monoid
    #:mzero
    #:mplus
    #:msum)
  (:import-from
    :fcl.monad+
    #:guard)
  (:export
    #:state
    #:run-state
    #:get-state
    #:put-state
    #:modify-state
    #:pop-state
    #:push-state
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet
    #:mzero
    #:mplus
    #:msum
    #:guard))

(in-package fcl.dt.state)


;;; Definition
(defdata state
  (%state function))

(defun run-state (state s)
  (check-type state state)
  (funcall (%state%0 state) s))


;;; Functor, Applicative and Monad
(defmethod unit ((class (eql 'state)) a)
  (%state (lambda (s) (values a s))))

(defmethod fmap (a->b (a* state))
  (check-type a->b function)
  (%state
    (lambda (s0)
      (multiple-value-bind (a s1) (run-state a* s0)
        (values (funcall a->b a) s1)))))

(defmethod amap (a->*b (a* state))
  (check-type a->*b state)
  (%state
    (lambda (s0)
      (multiple-value-bind (a->b s1) (run-state a->*b s0)
        (multiple-value-bind (a s2) (run-state a* s1)
          (values (funcall a->b a) s2))))))

(defmethod mmap (a->b* (a* state))
  (check-type a->b* function)
  (%state
    (lambda (s0)
      (multiple-value-bind (a s1) (run-state a* s0)
        (run-state (funcall a->b* a) s1)))))


;;; General Utility
(defun get-state ()
  (%state (lambda (s) (values s s))))

(defun put-state (s)
  (%state (lambda (_) (declare (ignore _)) (values nil s))))

(defun modify-state (function)
  (check-type function function)
  (monad-do (:in a (get-state))
            (put-state (funcall function a))
            (get-state)))

(defun pop-state ()
  (%state (lambda (s) (values (first s) (rest s)))))

(defun push-state (x)
  (%state (lambda (s) (values nil (cons x s)))))
