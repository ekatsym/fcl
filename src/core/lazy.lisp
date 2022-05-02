(defpackage fcl.lazy
  (:nicknames :fcl.core.lazy :fcl.lz)
  (:use :common-lisp)
  (:export #:promise #:delay #:force))
(in-package :fcl.lazy)


(defstruct (promise (:constructor make-promise (thunk))
                    (:copier nil)
                    (:predicate nil))
  (thunk thunk :read-only t)
  (forced? nil :type boolean)
  (cache nil))

(defmethod print-object ((object promise) stream)
  (with-slots (forced? cache) object
    (if forced?
        (format stream "#<PROMISE ~S>" cache)
        (format stream "#<PROMISE UNFORCED>"))))

(defmacro delay (expression)
  `(make-promise (lambda () ,expression)))

(defun force (promise)
  (check-type promise promise)
  (with-slots (thunk forced? cache) promise
    (unless forced?
      (setf cache (funcall thunk))
      (setf forced? t))
    cache))
