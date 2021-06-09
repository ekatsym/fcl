(defpackage fcl.state
  (:nicknames :fcl.data.state :fcl.st)
  (:use :common-lisp :fcl.monad)
  (:import-from
    :fcl.adata
    #:defdata)
  (:import-from
    :fcl.match
    #:ematch)
  (:export
    #:state #:run-state
    #:getst #:setst
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo))
(in-package :fcl.state)


;;; Definition
(defdata state
  (%state function))

(defun run-state (state s)
  (ematch state
    ((%state s->a&s) (funcall s->a&s s))))

(defun getst ()
  (%state (lambda (s) (values s s))))

(defun setst (s)
  (%state (lambda (s0) (declare (ignore s0)) (values nil s))))

(defun dost (expression)
  (declare (ignore expression))
  (unit 'state nil))


;;; Monad
(defmethod unit ((class (eql 'state)) a)
  (%state (lambda (s) (values s a))))

(define-fmap-by-monad state)

(define-amap-by-monad state)

(defmethod mmap (a->b* (a* state))
  (%state
    (lambda (s0)
      (ematch a*
        ((%state s->a&s)
         (multiple-value-bind (a s1) (funcall s->a&s s0)
           (run-state (funcall a->b* a) s1)))))))
