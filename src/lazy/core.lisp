(defpackage :fcl.lazy.core
  (:nicknames :fcl.l.core)
  (:use :common-lisp)
  (:export #:promise
           #:delay
           #:force))
(in-package :fcl.l.core)


(defstruct (promise (:constructor make-promise (thunk))
                    (:copier nil)
                    (:predicate nil))
  (thunk thunk :read-only t)
  (forced? nil :type boolean)
  (cache nil))

(defmethod print-object ((object promise) stream)
  (format stream "#<PROMISE ~S>" (force object)))

(defmacro delay (expression)
  `(make-promise (lambda () ,expression)))

(defun force (promise)
  (check-type promise promise)
  (with-slots (thunk forced? cache) promise
    (unless forced?
      (setf cache (funcall thunk))
      (setf forced? t))
    cache))
