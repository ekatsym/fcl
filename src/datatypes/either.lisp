(defpackage fcl.datatypes.either
  (:nicknames fcl.dt.either)
  (:use :common-lisp)
  (:import-from
    :fcl.data
    #:defdata)
  (:export
    #:either
    #:left
    #:right))
(in-package :fcl.datatypes.either)


(defdata either
  (left t)
  (right t))
