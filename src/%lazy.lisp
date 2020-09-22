(defpackage fcl.lazy
  (:use :common-lisp)
  (:import-from
    :fcl
    #:delay
    #:force)
  (:export
    #:delay
    #:force
    #:promise))
(in-package :fcl.lazy)


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
