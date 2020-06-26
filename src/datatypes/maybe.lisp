(defpackage fcl.datatypes.maybe
  (:use :common-lisp)
  (:import-from
    :fcl.defdata
    #:defdata))
(in-package fcl.datatypes.maybe)


(defdata maybe
  (nothing)
  (just t))
