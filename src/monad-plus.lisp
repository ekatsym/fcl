(defpackage fcl.monad+
  (:use :common-lisp)
  (:import-from
    :fcl
    #:guard)
  (:import-from
    :fcl.monad
    #:unit)
  (:import-from
    :fcl.monoid
    #:mzero)
  (:export
    #:guard))
(in-package :fcl.monad+)


(defun guard (class test)
  (if test
      (unit class nil)
      (mzero class)))
